use std::collections::HashMap;

mod parse;
pub use parse::json;

#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Object(HashMap<String, JsonValue>),
    Array(Vec<JsonValue>),
    String(String),
    Number(f32),
    Bool(bool),
    Null,
}

impl JsonValue {
    fn pretty_print(&self, indent_level: u16) -> String {
        let indent = String::from_iter(vec!['\t'; indent_level as usize].into_iter());
        match self {
            JsonValue::Object(map) => {
                let mut res = String::new();
                res.push_str("{\n");

                for (i, (k, v)) in map.iter().enumerate() {
                    res.push_str(&format!("\t{}{k}: {}", indent, v.pretty_print(indent_level + 1)));
                    if i != map.len() - 1 {
                        res.push(','); 
                    } 
                    res.push('\n');
                }

                res.push_str(&indent);
                res.push('}');
                res
            }
            
            JsonValue::Array(vec) => {
                let mut res = String::new();
                res.push_str("[\n");

                for (i, elem) in vec.iter().enumerate() {
                    res.push_str(&format!("\t{}{}", indent, elem.pretty_print(indent_level + 1)));

                    if i != vec.len() - 1 {
                        res.push(',');
                    }
                    res.push('\n');
                }

                res.push_str(&indent);
                res.push(']');
                res
            }

            JsonValue::String(s) => format!("\"{s}\""),

            JsonValue::Number(x) => format!("{x}"),

            JsonValue::Bool(b) => format!("{b}"),

            JsonValue::Null => format!("null"),
        }
    }
}

impl std::fmt::Display for JsonValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.pretty_print(0))
    }
}
