use std::fs::read_to_string;

use json_parser::json;

fn main() {
    for filename in ["tests/example_1.json", "tests/example_2.json"] {
        let test_file = read_to_string(filename).unwrap();
        let test_json = json(&test_file).unwrap();

        println!("{}", test_json);
    }
}
