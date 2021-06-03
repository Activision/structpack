use clap::{App, Arg};
use failure::{format_err, Error};
use gimli::{
    DW_AT_byte_size, DW_AT_count, DW_AT_data_member_location, DW_AT_name, DW_AT_type,
    DW_TAG_array_type, DW_TAG_base_type, DW_TAG_class_type, DW_TAG_const_type, DW_TAG_pointer_type,
    DW_TAG_structure_type, DW_TAG_subrange_type, DebugAbbrev, DebugInfo, DebugStr, LittleEndian,
};
use object::Object;

use std::collections::HashMap;

fn get_dwarf_string<R: gimli::Reader>(
    debug_str: &DebugStr<R>,
    attribute: gimli::Result<Option<gimli::AttributeValue<R>>>,
) -> Option<String> {
    if let Ok(Some(gimli::AttributeValue::DebugStrRef(offset))) = attribute {
        if let Ok(s) = debug_str.get_str(offset) {
            if let Ok(s) = s.to_string() {
                return Some(s.to_string());
            }
        }
    }
    None
}

fn get_dwarf_usize<R: gimli::Reader>(
    attribute: gimli::Result<Option<gimli::AttributeValue<R>>>,
) -> Option<usize> {
    match attribute {
        Ok(Some(gimli::AttributeValue::Udata(data))) => Some(data as usize),
        Ok(Some(gimli::AttributeValue::Data1(array))) => Some(u8::from_le_bytes(array) as usize),
        Ok(Some(gimli::AttributeValue::Data2((array, _)))) => {
            Some(u16::from_le_bytes(array) as usize)
        }
        Ok(Some(gimli::AttributeValue::Data4((array, _)))) => {
            Some(u32::from_le_bytes(array) as usize)
        }
        Ok(Some(gimli::AttributeValue::Data8((array, _)))) => Some(usize::from_le_bytes(array)),
        _ => None,
    }
}

#[derive(Clone, Debug)]
struct CppMember {
    name: String,
    type_name: String,
    type_size: usize,
    byte_offset: usize,
    array_count: Vec<usize>,
}

impl CppMember {
    fn total_size(&self) -> usize {
        let mut val = self.type_size;
        for x in &self.array_count {
            val *= x;
        }
        val
    }
}
impl std::fmt::Display for CppMember {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut array = String::new();
        for x in &self.array_count {
            array += &format!("[{}]", x);
        }
        writeln!(
            f,
            "    {} {}{}; // {} bytes",
            self.type_name,
            self.name,
            array,
            self.total_size()
        )?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct CppStruct {
    name: String,
    size: usize,
    members: Vec<CppMember>,
}

impl std::fmt::Display for CppStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "struct {}", self.name)?;
        writeln!(f, "{{",)?;
        for m in &self.members {
            write!(f, "{}", m)?;
        }
        writeln!(f, "}};")?;
        Ok(())
    }
}

fn struct_from_entry<'abbrev, 'unit, R, Offset>(
    debug_str: &gimli::DebugStr<R>,
    e: &gimli::DebuggingInformationEntry<'abbrev, 'unit, R, Offset>,
) -> CppStruct
where
    R: gimli::Reader<Offset = Offset> + 'unit,
    Offset: gimli::ReaderOffset + 'unit,
{
    CppStruct {
        name: get_dwarf_string(&debug_str, e.attr_value(DW_AT_name)).unwrap_or_default(),
        size: get_dwarf_usize(e.attr_value(DW_AT_byte_size)).unwrap_or_default(),
        members: Vec::new(),
    }
}

fn member_from_entry<'abbrev, 'unit, R, Offset>(
    compilation_unit: &'unit gimli::CompilationUnitHeader<R, Offset>,
    abbrev: &'abbrev gimli::Abbreviations,
    debug_str: &gimli::DebugStr<R>,
    mut e: gimli::DebuggingInformationEntry<'abbrev, 'unit, R, Offset>,
) -> CppMember
where
    R: gimli::Reader<Offset = Offset> + 'unit,
    Offset: gimli::ReaderOffset + 'unit,
{
    let name = get_dwarf_string(&debug_str, e.attr_value(DW_AT_name)).unwrap_or_default();
    let byte_offset = get_dwarf_usize(e.attr_value(DW_AT_data_member_location)).unwrap_or_default();

    // We must chase nested typedefs until we get the actual size,
    // but we want the top-level name to be the one that we save.
    let mut type_name = String::new();
    let mut type_size = 0;
    let mut is_pointer = false;
    let mut is_const = false;
    let mut array_count = Vec::new();
    while let Ok(Some(gimli::AttributeValue::UnitRef(unit_offset))) = e.attr_value(DW_AT_type) {
        if let Ok(mut cursor) = compilation_unit.entries_at_offset(abbrev, unit_offset) {
            let _ = cursor.next_entry();
            e = cursor.current().unwrap().clone();
            if type_name.is_empty() {
                type_name =
                    get_dwarf_string(&debug_str, e.attr_value(DW_AT_name)).unwrap_or_default();
            }
            #[allow(non_upper_case_globals)]
            match e.tag() {
                DW_TAG_pointer_type => is_pointer = true,
                DW_TAG_const_type => is_const = true,
                DW_TAG_array_type => {
                    let mut array_cursor = cursor.clone();
                    while let Ok(Some((delta_depth, array_child))) = array_cursor.next_dfs() {
                        if delta_depth < 0 {
                            break;
                        }
                        if array_child.tag() == DW_TAG_subrange_type {
                            array_count.push(
                                get_dwarf_usize(array_child.attr_value(DW_AT_count))
                                    .unwrap_or_default(),
                            );
                        }
                    }
                }
                DW_TAG_structure_type | DW_TAG_base_type | DW_TAG_class_type => {
                    type_size = get_dwarf_usize(e.attr_value(DW_AT_byte_size)).unwrap_or_default();
                }
                _ => {}
            }
        } else {
            break;
        }
    }
    if is_pointer {
        if type_name.is_empty() {
            type_name = "void".to_string();
        }
        type_name += "*";
        type_size = 8;
    }
    if is_const {
        type_name = format!("const {}", type_name);
    }
    CppMember {
        name,
        type_name,
        type_size,
        byte_offset,
        array_count,
    }
}

fn add_padding_blocks(s: &mut CppStruct) -> bool {
    let mut expected_offset = 0;
    let mut last_offset = 0;
    let mut new_members = Vec::with_capacity(s.members.len() * 2);
    for m in &s.members {
        let type_size = m.total_size();
        if type_size == 0 {
            return false;
        }

        if expected_offset == m.byte_offset {
            expected_offset += type_size;
        } else if m.byte_offset > expected_offset {
            new_members.push(CppMember {
                name: String::new(),
                type_name: "<padding>".to_string(),
                type_size: m.byte_offset - expected_offset,
                byte_offset: expected_offset,
                array_count: Vec::new(),
            });
            expected_offset = m.byte_offset + type_size;
        } else if m.byte_offset == last_offset {
            // This is a union...
            expected_offset = std::cmp::max(expected_offset, m.byte_offset + type_size);
        }
        new_members.push(m.clone());
        last_offset = m.byte_offset;
    }
    if expected_offset < s.size {
        new_members.push(CppMember {
            name: String::new(),
            type_name: "<padding>".to_string(),
            type_size: s.size - expected_offset,
            byte_offset: expected_offset,
            array_count: Vec::new(),
        });
    }
    s.members = new_members;
    true
}

fn align_up(amount: usize, align: usize) -> usize {
    let remainder = align - amount % align;
    amount + remainder
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn align() {
        assert_eq!(align_up(42, 4), 44);
    }
}

fn count_padding(s: &CppStruct) -> (usize, usize) {
    let mut max_base_size = 1;
    let mut padding = 0;
    let mut only_last_member = false;
    for (i, m) in s.members.iter().enumerate() {
        if m.type_name == "<padding>" {
            if i == s.members.len() - 1 && padding == 0 {
                only_last_member = true;
            }
            padding += m.type_size;
        } else {
            let base_size = if m.type_size > 8 {
                if m.type_size % 4 == 0 {
                    4
                } else {
                    8
                }
            } else {
                m.type_size
            };
            max_base_size = std::cmp::max(max_base_size, base_size);
        }
    }
    if only_last_member {
        // If only the last member is padded, we can't improve on this regardless
        // of our guess.
        (padding, s.size)
    } else {
        let optimal_size = align_up(s.size - padding, max_base_size);
        (padding, optimal_size)
    }
}

fn main() -> Result<(), Error> {
    let matches = App::new("structpack")
        .version("1.0")
        .about("Return structure packing info from an elf file.")
        .arg(
            Arg::with_name("file")
                .help("The name of a local file to be inspected")
                .short("f")
                .long("file")
                .value_name("FILENAME")
                .takes_value(true)
                .required(false),
        )
        .get_matches();

    let filename = matches.value_of("file").unwrap();
    println!("Investigating {}...", filename);

    let data = std::fs::read(filename)?;
    let obj = object::File::parse(&data).map_err(|e| format_err!("{}", e))?;
    let debug_info_data = obj
        .section_data_by_name(".debug_info")
        .ok_or_else(|| format_err!("Could not read .debug_info section data from elf file"))?;
    let debug_info = DebugInfo::new(&debug_info_data, LittleEndian);

    let debug_abbrev_data = obj
        .section_data_by_name(".debug_abbrev")
        .ok_or_else(|| format_err!("Could not read .debug_abbrev section data from elf file"))?;
    let debug_abbrev = DebugAbbrev::new(&debug_abbrev_data, LittleEndian);

    let debug_str_data = obj
        .section_data_by_name(".debug_str")
        .ok_or_else(|| format_err!("Could not read .debug_str section data from elf file"))?;
    let debug_str = DebugStr::new(&debug_str_data, LittleEndian);

    let mut structs: Vec<CppStruct> = Vec::new();

    let mut structs_seen: HashMap<String, (usize, usize)> = HashMap::new();

    let mut iter = debug_info.units();
    while let Ok(Some(unit)) = iter.next() {
        if let Ok(abbrev) = unit.abbreviations(&debug_abbrev) {
            let mut cursor = unit.entries(&abbrev);
            let _ = cursor.next_dfs();
            while let Some((_, entry)) = cursor.next_dfs()? {
                if (entry.tag() == DW_TAG_structure_type || entry.tag() == DW_TAG_class_type)
                    && entry.has_children()
                {
                    let mut cpp_struct = struct_from_entry(&debug_str, &entry);
                    if cpp_struct.name.is_empty() || cpp_struct.size == 0 {
                        continue;
                    }
                    if structs_seen.get(&cpp_struct.name).is_some() {
                        continue;
                    }
                    if let Some((_, first_member_entry)) = cursor.next_dfs()? {
                        cpp_struct.members.push(member_from_entry(
                            &unit,
                            &abbrev,
                            &debug_str,
                            first_member_entry.clone(),
                        ));
                        while let Some(member_entry) = cursor.next_sibling()? {
                            cpp_struct.members.push(member_from_entry(
                                &unit,
                                &abbrev,
                                &debug_str,
                                member_entry.clone(),
                            ));
                        }
                    }
                    if add_padding_blocks(&mut cpp_struct) {
                        let (padding, optimal_size) = count_padding(&cpp_struct);
                        if padding != 0 && cpp_struct.size != optimal_size {
                            let optimal_padding = optimal_size - (cpp_struct.size - padding);
                            println!(
                                "{} - {} bytes, {} padding (optimal size should be {} bytes, {} padding)",
                                cpp_struct.name, cpp_struct.size, padding, optimal_size, optimal_padding
                            );
                            println!("{}", cpp_struct);
                        }
                        structs_seen.insert(cpp_struct.name.clone(), (padding, cpp_struct.size));
                        structs.push(cpp_struct);
                    }
                }
            }
        }
    }

    println!("found {} structs", structs.len());

    Ok(())
}
