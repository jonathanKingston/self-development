use form_builder::{document, Element};
use std::collections::HashMap;

macro_rules! hashmap {
    ($( $key:tt : $value:expr ),*) => {
      {
        let mut hash = HashMap::new();
        $( hash.insert($key.to_string(), $value.to_string()); )*
        hash
      }
    };
}

#[derive(Serialize, Deserialize)]
pub enum BodyContent {
    String(String),
    Element(AnElement),
    Input(TextField),
}

impl From<String> for BodyContent {
    fn from(content: String) -> Self {
        BodyContent::String(content)
    }
}

impl From<&'static str> for BodyContent {
    fn from(content: &'static str) -> Self {
        BodyContent::String(content.to_string())
    }
}

impl From<AnElement> for BodyContent {
    fn from(content: AnElement) -> Self {
        BodyContent::Element(content)
    }
}

// TODO this is a hack we need a trait for elements
impl From<TextField> for BodyContent {
    fn from(content: TextField) -> Self {
        BodyContent::Input(content)
    }
}

pub trait El {}

/*Called AnElement as Element is used above */
#[derive(Serialize, Deserialize)]
pub struct AnElement {
    tag_name: String,
    attributes: HashMap<String, String>,
    body: Vec<BodyContent>,
}

impl El for AnElement {}

impl AnElement {
    pub fn create(
        tag_name: &str,
        attributes: HashMap<String, String>,
        body: Vec<BodyContent>,
    ) -> AnElement {
        AnElement {
            tag_name: tag_name.into(),
            attributes,
            body,
        }
    }

    pub fn build(&self) -> Element {
        let el = document.createElement(&self.tag_name);
        for (key, value) in &self.attributes {
            el.set_attribute(&key, &value);
        }
        for value in &self.body {
            el.append_child(match value {
                BodyContent::Element(el) => el.build(),
                BodyContent::Input(el) => el.build(),
                BodyContent::String(stringy) => document.createTextNode(stringy),
            });
        }
        el
    }
}

#[derive(Serialize, Deserialize)]
pub struct TextField {
    pub maxlength: i32,
    pub name: String,
    pub value: String,
}

impl TextField {
    fn build(&self) -> Element {
        let el = AnElement::create(
            "input",
            hashmap!{
              "maxlength": self.maxlength,
              "name": &self.name
            },
            vec![],
        );
        el.build()
    }
}

impl El for TextField {}

#[derive(Serialize, Deserialize)]
pub struct TextArea {
    pub name: String,
    pub value: String,
}

impl TextArea {
    pub fn build(&self) -> Element {
        let el = AnElement::create(
            "textarea",
            hashmap!{
              "name": &self.name
            },
            // TODO fix clone here?
            vec![self.value.clone().into()],
        );
        el.build()
    }
}
