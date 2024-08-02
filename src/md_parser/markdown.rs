use std::collections::HashMap;
use std::fmt;

type MatchesFn = fn(&str) -> bool;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    SingleValue(String),
    ListValue(Vec<String>),
}
impl Value {
    pub fn as_single_value(&self) -> Option<&String> {
        if let Value::SingleValue(ref s) = *self {
            Some(s)
        } else {
            None
        }
    }
    pub fn as_list_value(&self) -> Option<&Vec<String>> {
        if let Value::ListValue(ref v) = *self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Markdown {
    pub properties: HashMap<String, Value>,
    pub body: String,
}
impl Markdown {
    pub fn new() -> Self {
        Self {
            properties: HashMap::new(),
            body: String::from(""),
        }
    }
    pub fn from_body(c: &str) -> Self {
        Self {
            properties: HashMap::new(),
            body: String::from(c),
        }
    }
    pub fn from_properties(p: HashMap<String, Value>) -> Self {
        Self {
            properties: p,
            body: String::from(""),
        }
    }
    pub fn build(p: HashMap<String, Value>, c: &str) -> Self {
        Self {
            properties: p,
            body: String::from(c),
        }
    }
    pub fn has_properties(&self) -> bool {
        !self.properties.is_empty()
    }
    pub fn write_markdown(md: Markdown) -> String {
        format!("{}", md)
    }

    pub fn has_broken_properties(&self) -> bool {
        let mut valid = true;
        for (_, valtype) in &self.properties {
            match valtype {
                Value::SingleValue(v) => valid &= !Self::is_property_broken(&v),
                Value::ListValue(l) => valid &= !Self::is_list_property_broken(&l),
            }
        }
        !valid
    }
    fn is_property_broken(p: &str) -> bool {
        p.contains(":")
    }

    fn is_list_property_broken(p: &Vec<String>) -> bool {
        !p.iter().fold(true, |acc, t| acc && !Self::is_property_broken(&t))
    }

    pub fn fix_broken_properties(md: &Markdown) -> Markdown {
        let mut fixed = md.clone();
        if !md.has_broken_properties() {
            return fixed;
        }
        for (key, vtype) in &md.properties {
            match vtype {
                Value::SingleValue(v) => fixed
                    .properties
                    .insert(key.to_string(), Value::SingleValue(Self::fix_broken_property(&v))),
                Value::ListValue(l) => fixed.properties.insert(
                    key.to_string(),
                    Value::ListValue(Markdown::fix_broken_list_property(&l)),
                ),
            };
        }
        fixed
    }

    fn fix_broken_property(p: &str) -> String {
        p.replace(":", ";")
    }

    fn fix_broken_list_property(p: &Vec<String>) -> Vec<String> {
        p.iter().map(|e| Self::fix_broken_property(e)).collect()
    }
    pub fn input_has_properties(input: &str) -> bool {
        let input = remove_whitespace(input);
        first_line_matches(input, |l| l.eq("---"))
    }
}

impl fmt::Display for Markdown {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.has_properties() {
            let mut properties = String::new();
            writeln!(f, "---");
            let mut keys: Vec<_> = self.properties.keys().collect();
            keys.sort();
            for key in keys {
                let valtype = self.properties.get(key).unwrap();
                match valtype {
                    Value::SingleValue(v) => {
                        writeln!(f, "{}: {}", key, v);
                    }
                    Value::ListValue(l) => {
                        writeln!(f, "{}:", key);
                        for v in l {
                            writeln!(f, " - {}", v);
                        }
                    }
                }
            }
            writeln!(f, "---");
            write!(f, "{}", properties);
        }
        write!(f, "{}", self.body)
    }
}

fn remove_whitespace<'a>(input: &'a str) -> &'a str {
    let mut input = input.clone();
    while first_line_matches(input, |l| l.trim().is_empty()) {
        input = remove_first_line(input);
    }
    input
}

fn first_line_matches(input: &str, f: MatchesFn) -> bool {
    if input.is_empty() {
        return false;
    };
    input
        .lines()
        .take(1)
        .map(|l| f(l))
        .fold(true, |acc, l| acc && l)
}

fn remove_first_line<'a>(s: &'a str) -> &'a str {
    remove_first_n_lines(s, 1)
}

fn remove_first_n_lines(s: &str, n: usize) -> &str {
    if n == 0 {
        return s;
    }
    if let Some(pos) = s.match_indices('\n').skip(n - 1).next().map(|(i, _)| i) {
        &s[(pos + 1)..]
    } else {
        s
    }
}

#[cfg(test)]
mod tests {
    use crate::md_parser::{Markdown, Value};
    use std::collections::HashMap;  
    #[test]
    fn fix_properties() {
        let mut md = Markdown::new();
        md.body = "hello\nthis is the body of the text\nGoodbye!".to_owned();
        md.properties.insert("id".to_owned(), Value::SingleValue("Hello:there!".to_owned()));
        assert!(md.has_broken_properties());
        let fixed = Markdown::fix_broken_properties(&md);
        let exp = "---\nid: Hello;there!\n---\nhello\nthis is the body of the text\nGoodbye!";

        let res = Markdown::write_markdown(fixed.clone());
        assert_eq!(res, exp);
    }
    #[test]
    fn fix_broken_properties_in_list_aliases() {
        let exp = "---\n\
                     aliases:\n - statement; Greeting\n - statement Hi\n\
                     author: some guy\n\
                     id: test\n\
                     tags:\n - RSS\n - RKPRCP\n\
                     title: Statement Hi there!\n\
                     ---\n\
                     hello body";
        let mut md = Markdown::new();
        md.body = "hello body".to_owned();
        md.properties.insert("id".to_owned(), Value::SingleValue("test".to_owned()));
        md.properties.insert("author".to_owned(), Value::SingleValue("some guy".to_owned()));
        md.properties.insert("title".to_owned(), Value::SingleValue("Statement Hi there!".to_owned()));
        md.properties.insert("aliases".to_owned(), Value::ListValue(vec!["statement: Greeting".to_owned(), "statement Hi".to_owned()]));
        md.properties.insert("tags".to_owned(), Value::ListValue(vec!["RSS".to_owned(), "RKPRCP".to_owned()]));
        let is_broken = md.has_broken_properties();
        assert!(is_broken);
        let fixed = format!("{}", Markdown::fix_broken_properties(&md));
        assert_eq!(fixed, exp);
    }
    #[test]
    fn fix_broken_properties_in_list_tags() {
        let exp = "---\n\
                     author: some guy\n\
                     id: test\n\
                     tags:\n - RSS\n - RKP;RCP\n\
                     title: Statement Hi there!\n\
                     ---\n\
                     hello body";
        let mut md = Markdown::new();
        md.body = "hello body".to_owned();
        md.properties.insert("author".to_owned(), Value::SingleValue("some guy".to_owned()));
        md.properties.insert("id".to_owned(), Value::SingleValue("test".to_owned()));
        md.properties.insert("title".to_owned(), Value::SingleValue("Statement Hi there!".to_owned()));
        md.properties.insert("tags".to_owned(), Value::ListValue(vec!["RSS".to_owned(), "RKP:RCP".to_owned()]));
        let is_broken = md.has_broken_properties();
        assert!(is_broken);
        let fixed = format!("{}", Markdown::fix_broken_properties(&md));
        assert_eq!(fixed, exp);
    }

    #[test]
    fn fix_broken_properties_multiple() {
        let exp = "---\n\
                     author: some;; guy\n\
                     id: test\n\
                     title: Statement; Hi there!\n\
                     ---\n\
                     hello body";
        let mut md = Markdown::new();
        md.body = "hello body".to_owned();
        md.properties.insert("author".to_owned(), Value::SingleValue("some:: guy".to_owned()));
        md.properties.insert("id".to_owned(), Value::SingleValue("test".to_owned()));
        md.properties.insert("title".to_owned(), Value::SingleValue("Statement: Hi there!".to_owned()));
        let is_broken = md.has_broken_properties();
        assert!(is_broken);
        let fixed = format!("{}", Markdown::fix_broken_properties(&md));
        assert_eq!(fixed, exp);
    }
    #[test]
    fn fix_broken_properties_one() {
        let exp = "---\n\
                     author: some guy\n\
                     id: test\n\
                     tags:\n - RSS\n - RKP\n\
                     title: Statement; Hi there!\n\
                     ---\n\
                     hello body";
        let mut md = Markdown::new();
        md.body = "hello body".to_owned();
        md.properties.insert("author".to_owned(), Value::SingleValue("some guy".to_owned()));
        md.properties.insert("id".to_owned(), Value::SingleValue("test".to_owned()));
        md.properties.insert("title".to_owned(), Value::SingleValue("Statement: Hi there!".to_owned()));
        md.properties.insert("tags".to_owned(), Value::ListValue(vec!["RSS".to_owned(), "RKP".to_owned()]));
        let is_broken = md.has_broken_properties();
        assert!(is_broken);
        let fixed = format!("{}", Markdown::fix_broken_properties(&md));
        assert_eq!(fixed, exp);
    }

    // has_broken_properties()
    #[test]
    fn test_has_broken_properties_id_broken() {
        let mut md: HashMap<String, Value> = HashMap::new();

        md.insert(String::from("id"), Value::SingleValue("ahjgah:".to_owned()));
        md.insert(
            String::from("title"),
            Value::SingleValue("heading".to_owned()),
        );
        md.insert(
            String::from("author"),
            Value::SingleValue("writer".to_owned()),
        );
        md.insert(
            String::from("tags"),
            Value::ListValue(vec![
                "tag1".to_owned(),
                "tag2".to_owned(),
                "tag3".to_owned(),
            ]),
        );
        md.insert(
            String::from("date_saved"),
            Value::SingleValue("today".to_owned()),
        );
        md.insert(
            String::from("date_published"),
            Value::SingleValue("tomorrow".to_owned()),
        );
        md.insert(
            String::from("aliases"),
            Value::ListValue(vec!["al1".to_owned(), "al2".to_owned()]),
        );

        let markdown = Markdown::from_properties(md);
        assert!(markdown.has_broken_properties());
    }

    #[test]
    fn test_has_broken_properties_all_none() {
        let md = Markdown::new();
        assert!(!md.has_broken_properties());
    }
    #[test]
    fn test_has_no_broken_properties() {
        let mut md: HashMap<String, Value> = HashMap::new();

        md.insert(String::from("id"), Value::SingleValue("ahjgah".to_owned()));
        md.insert(
            String::from("title"),
            Value::SingleValue("heading".to_owned()),
        );
        md.insert(
            String::from("author"),
            Value::SingleValue("writer".to_owned()),
        );
        md.insert(
            String::from("tags"),
            Value::ListValue(vec![
                "tag1".to_owned(),
                "tag2".to_owned(),
                "tag3".to_owned(),
            ]),
        );
        md.insert(
            String::from("date_saved"),
            Value::SingleValue("today".to_owned()),
        );
        md.insert(
            String::from("date_published"),
            Value::SingleValue("tomorrow".to_owned()),
        );
        md.insert(
            String::from("aliases"),
            Value::ListValue(vec!["al1".to_owned(), "al2".to_owned()]),
        );

        let markdown = Markdown::from_properties(md);
        assert!(!markdown.has_broken_properties());
    }
    #[test]
    fn test_has_broken_properties_one_broken_alias() {
        let mut md: HashMap<String, Value> = HashMap::new();
        md.insert(String::from("id"), Value::SingleValue("ahjgah".to_owned()));
        md.insert(
            String::from("title"),
            Value::SingleValue("heading".to_owned()),
        );
        md.insert(
            String::from("author"),
            Value::SingleValue("writer".to_owned()),
        );
        md.insert(
            String::from("tags"),
            Value::ListValue(vec![
                "tag1".to_owned(),
                "tag2".to_owned(),
                "tag3".to_owned(),
            ]),
        );
        md.insert(
            String::from("date_saved"),
            Value::SingleValue("today".to_owned()),
        );
        md.insert(
            String::from("date_published"),
            Value::SingleValue("tomorrow".to_owned()),
        );
        md.insert(
            String::from("aliases"),
            Value::ListValue(vec!["al1".to_owned(), "a:l2".to_owned()]),
        );
        let markdown = Markdown::from_properties(md);
        assert!(markdown.has_broken_properties());
    }
    #[test]
    fn test_has_broken_properties_one_broken_tag() {
        let mut md: HashMap<String, Value> = HashMap::new();
        md.insert(String::from("id"), Value::SingleValue("ahjgah".to_owned()));
        md.insert(
            String::from("title"),
            Value::SingleValue("heading".to_owned()),
        );
        md.insert(
            String::from("author"),
            Value::SingleValue("writer".to_owned()),
        );
        md.insert(
            String::from("tags"),
            Value::ListValue(vec![
                "tag1".to_owned(),
                "tag:2".to_owned(),
                "tag3".to_owned(),
            ]),
        );
        md.insert(
            String::from("date_saved"),
            Value::SingleValue("today".to_owned()),
        );
        md.insert(
            String::from("date_published"),
            Value::SingleValue("tomorrow".to_owned()),
        );
        md.insert(
            String::from("aliases"),
            Value::ListValue(vec!["al1".to_owned(), "al2".to_owned()]),
        );
        let markdown = Markdown::from_properties(md);
        assert!(markdown.has_broken_properties());
    }
    // has_properties()
    #[test]
    fn test_has_properties() {
        assert!(Markdown::input_has_properties("---\nhello"));
    }

    #[test]
    fn test_only_properties() {
        assert!(Markdown::input_has_properties("---"));
    }

    #[test]
    fn test_has_no_properties() {
        assert!(!Markdown::input_has_properties("hello"));
    }

    #[test]
    fn test_has_properties_empty() {
        assert!(!Markdown::input_has_properties(""));
    }

    #[test]
    fn test_has_properties_with_tabs() {
        assert!(Markdown::input_has_properties("    \n---\nhello"));
    }
    #[test]
    fn test_has_properties_with_whitespace() {
        assert!(Markdown::input_has_properties("    \n---\nhello"));
    }
    #[test]
    fn test_has_properties_with_newline() {
        assert!(Markdown::input_has_properties("\n---\nhello"));
    }
}
