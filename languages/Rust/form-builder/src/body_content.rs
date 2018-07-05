mod body_content {
#[derive(Serialize, Deserialize)]
enum BodyContent {
  String(String),
  Element(AnElement),
  Input(TextField),
}

impl From <String> for BodyContent {
  fn from(content: String) -> Self {
    BodyContent::String(content)
  }
}

impl From <&'static str> for BodyContent {
  fn from(content: &'static str) -> Self {
    BodyContent::String(content.to_string())
  }
}

impl From <AnElement> for BodyContent {
  fn from(content: AnElement) -> Self {
    BodyContent::Element(content)
  }
}

// TODO this is a hack we need a trait for elements
impl From <TextField> for BodyContent {
  fn from(content: TextField) -> Self {
    BodyContent::Input(content)
  }
}

}
