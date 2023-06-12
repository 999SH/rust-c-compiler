use std::collections::HashSet;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

/*
 * Include this to use preprocessor
 * let include_path = vec![
            Path::new("/usr/include"),
            Path::new("/usr/include/linux"),
            Path::new("/usr/include/c++/13.1.1"),
            Path::new("/usr/include/c++/13.1.1/bits"),
            Path::new("/usr/include/c++/13.1.1/tr1"),
            Path::new("/usr/include/c++/13.1.1/x86_64-pc-linux-gnu")
        ];
        let mut processed_files = HashSet::new();
        let preprocessed_content = preprocessor::preprocess(&file_path, &include_path, &mut processed_files);
*/

pub fn preprocess(
    file: &Path,
    include_path: &[&Path],
    processed_files: &mut HashSet<PathBuf>,
) -> String {
    if processed_files.contains(file) {
        return String::new(); // return empty string if file already processed
    }
    processed_files.insert(file.to_path_buf()); // add current file to set of processed files

    let mut content = String::new();
    fs::File::open(file)
        .expect("Could not open file")
        .read_to_string(&mut content)
        .expect("Could not read file");

    let mut result = String::new();
    let lines = content.lines();

    let mut definitions = std::collections::HashMap::new();
    let mut skip = false;

    for line in lines {
        let trimmed_line = line.trim_start();
        if trimmed_line.starts_with("#define") {
            let mut split = trimmed_line.split_whitespace().skip(1); // Skip '#define'
            if let Some(name) = split.next() {
                let value = split.collect::<Vec<&str>>().join(" ");
                definitions.insert(name.to_string(), value);
            }
        } else if trimmed_line.starts_with("#undef") {
            if let Some(name) = trimmed_line.split_whitespace().nth(1) {
                definitions.remove(name);
            }
        } else if trimmed_line.starts_with("#ifdef") {
            if let Some(name) = trimmed_line.split_whitespace().nth(1) {
                skip = !definitions.contains_key(name);
            }
        } else if trimmed_line.starts_with("#ifndef") {
            if let Some(name) = trimmed_line.split_whitespace().nth(1) {
                skip = definitions.contains_key(name);
            }
        } else if trimmed_line.starts_with("#endif") {
            skip = false;
        } else if trimmed_line.starts_with("#else") {
            skip = !skip;
        } else if trimmed_line.starts_with("#if defined") {
            if let Some(name) = trimmed_line.split_whitespace().nth(2) {
                skip = !definitions.contains_key(name);
            }
        } else if trimmed_line.starts_with("#if") {
            if let Some(condition) = trimmed_line.split_whitespace().nth(1) {
                let condition_value: i32 = condition.parse().unwrap_or(0);
                skip = condition_value == 0;
            }
        } else if trimmed_line.starts_with("#elif defined") {
            if skip {
                if let Some(name) = trimmed_line.split_whitespace().nth(2) {
                    skip = !definitions.contains_key(name);
                }
            }
        } else if trimmed_line.starts_with("#elif") {
            if skip {
                if let Some(condition) = trimmed_line.split_whitespace().nth(1) {
                    let condition_value: i32 = condition.parse().unwrap_or(0);
                    skip = condition_value == 0;
                }
            }
        } else if !trimmed_line.starts_with("#") && !skip {
            // skip other preprocessor directives
            let mut processed_line = trimmed_line.to_string();
            for (name, value) in &definitions {
                // Replace defined names with their values
                processed_line = processed_line.replace(name, value);
            }
            result.push_str(&processed_line);
            result.push('\n');
        } else if trimmed_line.starts_with("#include \"") {
            if let Some(include_file_name) = trimmed_line.split('"').nth(1) {
                let mut search_paths = vec![file.parent().unwrap()];
                search_paths.extend_from_slice(include_path);
                let include_file_path = find_file(include_file_name, &search_paths);
                let include_file_content =
                    preprocess(&include_file_path, include_path, processed_files);
                result.push_str(&include_file_content);
            }
        } else if trimmed_line.starts_with("#include <") {
            if let Some(include_file_name) = trimmed_line
                .split('<')
                .nth(1)
                .and_then(|s| s.split('>').next())
            {
                let include_file_path = find_file(include_file_name, include_path); // use include_path directly
                let include_file_content =
                    preprocess(&include_file_path, include_path, processed_files);
                result.push_str(&include_file_content);
            }
        } else {
            result.push_str(trimmed_line);
            result.push('\n');
        }
    }

    result
}

pub fn find_file(file_name: &str, paths: &[&Path]) -> PathBuf {
    for path in paths {
        let file_path = path.join(file_name);
        //println!("Looking for file: {:?}", file_name);
        //println!("Looking for file at: {:?}", file_path);
        if file_path.exists() {
            return file_path;
        }
    }
    panic!("File not found: {}", file_name);
}
