mod md_parser;
use crate::md_parser::MarkdownParser ;

fn main() {
    let parser = MarkdownParser::new("test".to_owned());
    let _markdown = parser.read_markdown();
    println!("Hello, world!");
}
