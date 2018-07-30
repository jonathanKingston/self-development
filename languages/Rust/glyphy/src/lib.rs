extern crate markdown;
extern crate yaml_rust;
use yaml_rust::{YamlLoader, Yaml};
use std::fs::File;
use std::io::prelude::*;


struct Page {
    front_matter: Option<Yaml>,
    content: String 
}

impl Page {
    fn get_attribute_str(&self, attribute: &str) -> Option<&str> {
        match self.front_matter {
            Some(ref front_matter) => {
                front_matter[attribute].as_str()
            },
            None => None,
        }
    }
}

pub fn parse_file(filename: &str) -> String {
    let mut f = File::open(filename).expect("file not found");

    let mut content = String::new();
    f.read_to_string(&mut content)
        .expect("something went wrong reading the file");

    // https://jekyllrb.com/docs/frontmatter/
    let separator = "---\n";
    let page = match content.starts_with(separator) {
        true => {
          let mut parts: Vec<&str> = content.split(separator).collect();
          let front_matter = Some(YamlLoader::load_from_str(parts[1]).unwrap()[0].clone());
          let content: Vec<&str> = parts.drain(2..).collect();
          Page {
            front_matter,
            content: content.join(separator),
          }
        },
        false => Page {
          front_matter: None,
          content,
        },
    };

    // TODO this should accept a template and then pass in the front matter
    if let Some(title) = page.get_attribute_str("title") {
      println!("<h1>{}</h1>", title);
    }
    markdown::to_html(&page.content)
}
