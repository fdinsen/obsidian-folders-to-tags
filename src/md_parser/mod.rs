use std::{iter::Peekable, mem, str::Lines};
use thiserror::Error;

pub mod markdown;

use crate::md_parser::markdown::{Markdown, Value};

type Key = String;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParserError {
    #[error("Property `{0}` did not have a value")]
    PropertyWithoutValue(String),
    #[error("Trying to parse with finished state")]
    ParsingFinishedState(),
    #[error("Parsed list item `{1}` for unknown key `{0}`")]
    ParsingListWithUnknownKey(String, String),
    #[error("Parsed single item `{0}` as list")]
    ParsingSingleAsList(String),
}

#[derive(Debug)]
pub enum ParserState<'a> {
    Start(),
    ReadProperties(Peekable<Lines<'a>>, Markdown),
    ReadSingle(String, String, Peekable<Lines<'a>>, Markdown),
    ReadList(Key, Peekable<Lines<'a>>, Markdown),
    ReadBody(Peekable<Lines<'a>>, Markdown),
    End(Markdown),
}

impl PartialEq for ParserState<'_> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MarkdownParser {
    input: String,
}

impl MarkdownParser {
    pub fn new(input: String) -> Self {
        Self { input }
    }
    fn parse_token<'a>(&'a self, state: ParserState<'a>) -> Result<ParserState<'a>, ParserError> {
        match state {
            ParserState::Start() => Self::handle_start(&self.input),
            ParserState::ReadProperties(lines, md) => Self::handle_read_properties(lines, md),
            ParserState::ReadSingle(key, value, lines, md) => {
                Self::handle_read_single(key, value, lines, md)
            }
            ParserState::ReadList(key, lines, md) => Self::handle_read_list(key, lines, md),
            ParserState::ReadBody(lines, md) => Self::handle_read_body(lines, md),
            ParserState::End(_) => Err(ParserError::ParsingFinishedState()),
        }
    }

    pub fn read_markdown(&self) -> Result<Markdown, ParserError> {
        if !Markdown::input_has_properties(&self.input) {
            return Ok(Markdown::from_body(&self.input));
        }
        let mut state = ParserState::Start();
        loop {
            state = self.parse_token(state)?;
            if let ParserState::End(md) = state {
                return Ok(md);
            }
        }
    }

    fn has_list_item(line: Option<&str>) -> bool {
        match line {
            Some(l) => 
                l.trim().starts_with("-") && !l.trim().eq("---"),
            None => false,
        }
    }

    fn handle_read_list<'a>(
        key: String,
        mut lines: Peekable<Lines>,
        mut md: Markdown,
    ) -> Result<ParserState, ParserError> {
        let line = match lines.peek().copied() {
            Some(l) if Self::has_list_item(Some(l)) => lines.next().unwrap(),
            _ => return Ok(ParserState::ReadProperties(lines, md)),
        };
        let value = line
            .split_once("-")
            .map(|(_, value)| value.trim())
            .unwrap(); // Function would have early returned if split_once would've failed
        let cur_val = md
            .properties
            .get(&key)
            .ok_or_else(|| ParserError::ParsingListWithUnknownKey(key.clone(), value.to_owned()))?;
        match cur_val {
            Value::ListValue(values) => {
                let mut new_values: Vec<String> = values.clone();
                new_values.push(value.to_owned());
                md.properties
                    .insert(key.clone(), Value::ListValue(new_values));
                Ok(ParserState::ReadList(key, lines, md)) // Try to continue reading list
            }
            Value::SingleValue(_) => Err(ParserError::ParsingSingleAsList(key)),
        }
    }

    fn handle_start(body: &String) -> Result<ParserState, ParserError> {
        let md = Markdown::new();
        let mut lines = body.lines().peekable();
        if Markdown::input_has_properties(body) {
            lines.next();
            Ok(ParserState::ReadProperties(lines, md))
        } else {
            Ok(ParserState::ReadBody(lines, md))
        }
    }

    fn handle_read_properties(
        mut lines: Peekable<Lines>,
        mut md: Markdown,
    ) -> Result<ParserState, ParserError> {
        let line: &str = match lines.next() {
            Some(l) if l.trim().is_empty() => {
                lines.next();
                return Ok(ParserState::ReadProperties(lines, md));
            }
            Some(l) if l.trim().eq("---") => {
                return Ok(ParserState::ReadBody(lines, md));
            }
            None => return Ok(ParserState::End(md)),
            Some(l) => l,
        };
        let (key, value) = match line.split_once(":") {
            Some(kv) => Self::trim_both(kv),
            None => return Err(ParserError::PropertyWithoutValue(line.to_owned())),
        };

        if value.is_empty() {
            md.properties
                .insert(key.to_string(), Value::ListValue(Vec::new()));
            Ok(ParserState::ReadList(key.to_owned(), lines, md))
        } else {
            Ok(ParserState::ReadSingle(
                key.to_owned(),
                value.to_owned(),
                lines,
                md,
            ))
        }
    }

    fn trim_both<'a>(key_value: (&'a str, &'a str)) -> (&'a str, &'a str) {
        let (key, value) = key_value;
        (key.trim(), value.trim())
    }

    fn handle_read_single<'a>(
        key: String,
        value: String,
        lines: Peekable<Lines<'a>>,
        mut md: Markdown,
    ) -> Result<ParserState<'a>, ParserError> {
        md.properties.insert(key, Value::SingleValue(value));
        Ok(ParserState::ReadProperties(lines, md))
    }

    fn handle_read_body(
        mut lines: Peekable<Lines>,
        mut md: Markdown,
    ) -> Result<ParserState, ParserError> {
        let should_skip = match lines.peek() {
            Some(l) => l.trim().eq("---"),
            None => false,
        };
        if should_skip {
            lines.next(); // skip "---"
        }
        md.body = lines.collect::<Vec<&str>>().join("\n");
        Ok(ParserState::End(md))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // use crate::md_parser::MarkdownParser;
    use crate::md_parser::{Markdown, Value};
    use std::collections::HashMap;

    #[test]
    fn write_new_list_properties() {
        let input = "hello\nthis is the body of the text\nGoodbye!";
        let input_has_properties = Markdown::input_has_properties(input);
        assert!(!input_has_properties);

        let parser = MarkdownParser::new(input.to_owned());
        let mut md = parser.read_markdown().unwrap();
        let mut new_props: HashMap<String, Value> = HashMap::new();
        new_props.insert(
            String::from("id"),
            Value::SingleValue(String::from("Hello!")),
        );
        new_props.insert(
            String::from("tags"),
            Value::ListValue(vec![String::from("tag1"), String::from("tag2")]),
        );
        md.properties = new_props;
        let exp = "---\nid: Hello!\ntags:\n - tag1\n - tag2\n---\nhello\nthis is the body of the text\nGoodbye!";

        let res = Markdown::write_markdown(md.clone());
        let parser = MarkdownParser::new(res.clone());
        let new_md = parser.read_markdown().unwrap();
        assert_eq!(res, exp);
        assert_eq!(new_md, md);
    }

    #[test]
    fn overwrite_existing_properties() {
        let input =
            "---\nid: test\ntitle: test\n---\nhello\nthis is the body of the text\nGoodbye!";
        let input_has_properties = Markdown::input_has_properties(input);
        assert!(input_has_properties);

        let parser = MarkdownParser::new(input.to_owned());
        let mut md = parser.read_markdown().unwrap();
        let mut new_props: HashMap<String, Value> = HashMap::new();
        new_props.insert(
            String::from("id"),
            Value::SingleValue(String::from("Hello!")),
        );
        md.properties = new_props;
        let exp = "---\nid: Hello!\n---\nhello\nthis is the body of the text\nGoodbye!";

        let res = Markdown::write_markdown(md.clone());
        let parser = MarkdownParser::new(res.clone());
        let new_md = parser.read_markdown().unwrap();
        assert_eq!(res, exp);
        assert_eq!(new_md, md);
    }

    #[test]
    fn write_new_properties() {
        let input = "hello\nthis is the body of the text\nGoodbye!";
        let input_has_properties = Markdown::input_has_properties(input);
        assert!(!input_has_properties);

        let parser = MarkdownParser::new(input.to_owned());
        let mut md = parser.read_markdown().unwrap();
        let mut new_props: HashMap<String, Value> = HashMap::new();
        new_props.insert(
            String::from("id"),
            Value::SingleValue(String::from("Hello!")),
        );
        md.properties = new_props;
        let exp = "---\nid: Hello!\n---\nhello\nthis is the body of the text\nGoodbye!";

        let res = Markdown::write_markdown(md.clone());
        let parser = MarkdownParser::new(res.clone());
        let new_md = parser.read_markdown().unwrap();
        assert_eq!(res, exp);
        assert_eq!(new_md, md);
    }

    #[test]
    fn test_properties_to_string_full() {
        let input = "---\n\
                    aliases:\n - History is moving\n - in our direction\n - today\n\
                    author: RCP Central Committee\n\
                    date_published: 2024-07-16 18:35:56\n\
                    date_saved: 2024-07-16 18:35:56\n\
                    id: d576bc03-058c-4d0f-aa64-a6a5a5e6ea55\n\
                    tags:\n - RSS\n - The_Communist\n\
                    title: RCP Central Committee statement: “History is moving in our direction” | The Communist\n\
                    ---\n";
        let parser = MarkdownParser::new(input.to_owned());
        let prop = parser.read_markdown().unwrap();
        if let Value::ListValue(tags) = prop.properties.get("tags").unwrap() {
            assert_eq!(tags[0], "RSS");
            let output = format!("{}", prop);
            assert_eq!(input, output);
        } else {
            assert!(false);
        }
    }
    #[test]
    fn test_properties_to_string_skip_some() {
        let input = "---\n\
                    date_published: 2024-07-16 18:35:56\n\
                    date_saved: 2024-07-16 18:35:56\n\
                    id: d576bc03-058c-4d0f-aa64-a6a5a5e6ea55\n\
                    tags:\n - RSS\n - The_Communist\n\
                    ---\n";
        let parser = MarkdownParser::new(input.to_owned());
        let prop = parser.read_markdown().unwrap();
        if let Value::ListValue(tags) = prop.properties.get("tags").unwrap() {
            assert_eq!(tags[0], "RSS");
            let output = format!("{}", prop);
            assert_eq!(input, output);
        } else {
            assert!(false);
        }
    }

    // read_markdown()
    #[test]
    fn test_read_markdown_none() {
        let input = "---\n  \n---";
        let parser = MarkdownParser::new(input.to_owned());
        let md = parser.read_markdown().unwrap();
        assert!(!md.has_properties());
    }

    #[test]
    fn test_read_markdown_tags_and_aliases() {
        let input = "---\ntags:\n- RSS\n- RKP\naliases:\n- one\n- two\n- three\nid: test\n---";
        let parser = MarkdownParser::new(input.to_owned());
        let md = parser.read_markdown().unwrap();
        assert!(md.properties.contains_key("tags"));
        assert!(md.properties.contains_key("aliases"));
        assert!(md.properties.contains_key("id"));
        if let Value::ListValue(tags) = md.properties.get("tags").unwrap() {
            assert_eq!(tags.len(), 2);
            assert_eq!(tags[0], "RSS");
            assert_eq!(tags[1], "RKP");
        } else {
            assert!(false);
        }
        if let Value::ListValue(aliases) = md.properties.get("aliases").unwrap() {
            assert_eq!(aliases.len(), 3);
            assert_eq!(aliases[0], "one");
            assert_eq!(aliases[1], "two");
            assert_eq!(aliases[2], "three");
        } else {
            assert!(false);
        }
    }
    #[test]
    fn test_read_markdown_aliases() {
        let input = "---\naliases:\n- RSS\n- RKP\n---";
        let parser = MarkdownParser::new(input.to_owned());
        let md = parser.read_markdown().unwrap();
        assert!(md.properties.contains_key("aliases"));

        if let Value::ListValue(aliases) = md.properties.get("aliases").unwrap() {
            assert_eq!(aliases.len(), 2);
            assert_eq!(aliases[0], "RSS");
            assert_eq!(aliases[1], "RKP");
        } else {
            assert!(false);
        }
    }
    #[test]
    fn test_read_markdown_tags() {
        let input = "---\ntags:\n- RSS\n- RKP\n---";
        let parser = MarkdownParser::new(input.to_owned());
        let md = parser.read_markdown().unwrap();
        assert!(md.properties.contains_key("tags"));

        if let Value::ListValue(tags) = md.properties.get("tags").unwrap() {
            assert_eq!(tags.len(), 2);
            assert_eq!(tags[0], "RSS");
            assert_eq!(tags[1], "RKP");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_read_markdown_all_except_lists() {
        let input = "---\nid: di\ntitle: eltit\nauthor: rohtua\ndate_saved: devas\ndate_published: dehsilbup\n---";
        let parser = MarkdownParser::new(input.to_owned());
        let md: Markdown = parser.read_markdown().unwrap();
        assert_eq!(md.properties.len(), 5);
        assert!(md.properties.contains_key("id"));
        assert!(md.properties.contains_key("title"));
        assert!(md.properties.contains_key("author"));
        assert!(md.properties.contains_key("date_saved"));
        assert!(md.properties.contains_key("date_published"));
        assert_eq!(
            md.properties.get("id").unwrap().as_single_value().unwrap(),
            "di"
        );
        assert_eq!(
            md.properties
                .get("title")
                .unwrap()
                .as_single_value()
                .unwrap(),
            "eltit"
        );
        assert_eq!(
            md.properties
                .get("author")
                .unwrap()
                .as_single_value()
                .unwrap(),
            "rohtua"
        );
        assert_eq!(
            md.properties
                .get("date_saved")
                .unwrap()
                .as_single_value()
                .unwrap(),
            "devas"
        );
        assert_eq!(
            md.properties
                .get("date_published")
                .unwrap()
                .as_single_value()
                .unwrap(),
            "dehsilbup"
        );
    }

    #[test]
    fn test_read_markdown_no_prop_section() {
        let input = "hello\nhi\nthere";
        let parser = MarkdownParser::new(input.to_owned());
        let md = parser.read_markdown().unwrap();
        assert!(!md.has_properties());
    }
    fn get_test_markdown_parser(input: &str) -> MarkdownParser {
        MarkdownParser::new(input.to_string())
    }

    #[test]
    fn test_start_state_to_read_properties() {
        let input = "---\ntitle: Test\n---\nBody here";
        let parser = get_test_markdown_parser(input);
        let initial_state = ParserState::Start();

        let expected_md = Markdown::new();
        let mut lines = input.lines().peekable();

        let expected_state = ParserState::ReadProperties(lines, expected_md);

        let result = parser.parse_token(initial_state).unwrap();
        assert_eq!(result, expected_state);
    }

    #[test]
    fn test_handle_property_with_value() {
        let input = "---\ntitle: Test\n---\nBody here";
        let parser = get_test_markdown_parser(input);
        let mut lines = input.lines().peekable();
        lines.next(); // Skip the first line with ---
        let state = ParserState::ReadProperties(lines, Markdown::new());

        let result = parser.parse_token(state).unwrap();

        match result {
            ParserState::ReadSingle(key, value, _, _) => {
                assert_eq!(key, "title");
                assert_eq!(value, "Test");
            }
            _ => panic!("Expected ReadSingle state"),
        }
    }

    #[test]
    fn test_handle_property_without_value() {
        let input = "---\nitems:\n- Item 1\n- Item 2\n---\nBody here";
        let parser = get_test_markdown_parser(input);
        let mut lines = input.lines().peekable();
        lines.next(); // Skip the first line with ---
        let state = ParserState::ReadProperties(lines, Markdown::new());

        let result = parser.parse_token(state).unwrap();

        match result {
            ParserState::ReadList(key, _, _) => {
                assert_eq!(key, "items");
            }
            _ => panic!("Expected ReadList state"),
        }
    }

    #[test]
    fn test_handle_read_list() {
        let input = "---\nitems:\n- Item 1\n- Item 2\n---\nBody here";
        let parser = get_test_markdown_parser(input);
        let mut lines = input.lines().peekable();
        lines.next(); // Skip the first line with ---
        lines.next(); // assume already read first line
        let mut md = Markdown::new();
        md.properties
            .insert("items".to_string(), Value::ListValue(Vec::new()));
        let state = ParserState::ReadList("items".to_string(), lines, md.clone());

        let result = parser.parse_token(state).unwrap();

        match result {
            ParserState::ReadList(key, _, md) => {
                assert_eq!(key, "items");
                match md.properties.get(&key).unwrap() {
                    Value::ListValue(items) => {
                        assert_eq!(items, &vec!["Item 1".to_string()]);
                    }
                    _ => panic!("Expected ListValue"),
                }
            }
            _ => panic!("Expected ReadList state"),
        }
    }

    #[test]
    fn test_handle_read_body_no_properties() {
        let input = "Body here";
        let parser = get_test_markdown_parser(input);
        let mut lines = input.lines().peekable();

        let state = ParserState::ReadBody(lines, Markdown::new());

        let result = parser.parse_token(state).unwrap();

        match result {
            ParserState::End(md) => {
                assert_eq!(md.body, "Body here");
            }
            _ => panic!("Expected End state"),
        }
    }
    #[test]
    fn test_handle_read_body() {
        let input = "---\n---\nBody here";
        let parser = get_test_markdown_parser(input);
        let mut lines = input.lines().peekable();

        lines.next(); // Skip the second line with ---
        let state = ParserState::ReadBody(lines, Markdown::new());

        let result = parser.parse_token(state).unwrap();

        match result {
            ParserState::End(md) => {
                assert_eq!(md.body, "Body here");
            }
            _ => panic!("Expected End state"),
        }
    }

    #[test]
    fn test_parse_complete_markdown() {
        let input = "---\ntitle: Test\nitems:\n- Item 1\n- Item 2\n---\nBody here";
        let parser = get_test_markdown_parser(input);

        let result = parser.read_markdown().unwrap();

        let mut expected_md = Markdown::new();
        expected_md
            .properties
            .insert("title".to_string(), Value::SingleValue("Test".to_string()));
        expected_md.properties.insert(
            "items".to_string(),
            Value::ListValue(vec!["Item 1".to_string(), "Item 2".to_string()]),
        );
        expected_md.body = "Body here".to_string();

        assert_eq!(result, expected_md);
    }

    #[test]
    fn test_parse_without_properties() {
        let input = "Body here without properties";
        let parser = get_test_markdown_parser(input);

        let result = parser.read_markdown().unwrap();

        let expected_md = Markdown::from_body(input);
        assert_eq!(result, expected_md);
    }

    #[test]
    fn test_property_without_value_error() {
        let input = "---\ntitle\n---\nBody here";
        let parser = get_test_markdown_parser(input);
        let result = parser.read_markdown();

        assert_eq!(
            result,
            Err(ParserError::PropertyWithoutValue("title".to_string()))
        );
    }

    #[test]
    fn test_parsing_finished_state_error() {
        let input = "---\ntitle: Test\n---\nBody here";
        let parser = get_test_markdown_parser(input);

        let mut state = ParserState::End(Markdown::new());
        let result = parser.parse_token(state);

        assert_eq!(result, Err(ParserError::ParsingFinishedState()));
    }

    #[test]
    fn test_parsing_list_with_unknown_key_error() {
        let input = "---\nitems:\n- Item 1\n- Item 2\n---\nBody here";
        let parser = get_test_markdown_parser(input);
        let mut lines = input.lines().peekable();
        lines.next();
        lines.next(); // assume already read first property
        let mut md = Markdown::new();
        let state = ParserState::ReadList("unknown".to_string(), lines, md.clone());

        let result = parser.parse_token(state);

        assert_eq!(
            result,
            Err(ParserError::ParsingListWithUnknownKey(
                "unknown".to_string(),
                "Item 1".to_string()
            ))
        );
    }

    #[test]
    fn test_parsing_single_as_list_error() {
        let input = "---\ntitle: Test\n- List Item\n---\ntest";
        let parser = get_test_markdown_parser(input);
        let mut lines = input.lines().peekable();
        lines.next();
        lines.next(); // assume already read first property
        let mut md = Markdown::new();
        md.properties
            .insert("title".to_string(), Value::SingleValue("Test".to_string()));
        let state = ParserState::ReadList("title".to_string(), lines, md.clone());

        let result = parser.parse_token(state);

        assert_eq!(
            result,
            Err(ParserError::ParsingSingleAsList("title".to_string()))
        );
    }
}
