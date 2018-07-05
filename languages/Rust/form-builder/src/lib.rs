#![feature(proc_macro, wasm_custom_section, wasm_import_module)]
extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;

mod types;
pub use types::{BodyContent, AnElement, TextField};
use serde_json::Error;

pub mod form_builder {
use wasm_bindgen::prelude::*;
  #[wasm_bindgen]
  extern "C" {
      pub type HTMLDocument;
      pub static document: HTMLDocument;
      #[wasm_bindgen(method)]
      pub fn createElement(this: &HTMLDocument, tagName: &str) -> Element;
      #[wasm_bindgen(method)]
      pub fn createTextNode(this: &HTMLDocument, string: &str) -> Element;
      #[wasm_bindgen(method, getter)]
      pub fn body(this: &HTMLDocument) -> Element;
  
      pub type Element;
      #[wasm_bindgen(method, setter = textContent)]
      pub fn set_text_content(this: &Element, text: &str);
      #[wasm_bindgen(method, js_name = setAttribute)]
      pub fn set_attribute(this: &Element, text: &str, value: &str);
      #[wasm_bindgen(method, js_name = appendChild)]
      pub fn append_child(this: &Element, other: Element);
  }

}

use std::collections::{HashMap};

macro_rules! hashmap {
    ($( $key:tt : $value:expr ),*) => {
      {
        let mut hash = HashMap::new();
        $( hash.insert($key.to_string(), $value.to_string()); )*
        hash
      }
    };
}

// Called by our JS entry point to run the example
#[wasm_bindgen]
pub fn run() {
/*
    let val = document.createElement("label");
    val.set_text_content("Email");
    let input = document.createElement("input");
    input.set_text_content("Hello from Rust!");
    input.set_attribute("value", "Hello Rust!");
    input.set_attribute("type", "email");
    val.append_child(input);
    document.body().append_child(val);
*/

     // Eventually this will need a crate to validate: https://html.spec.whatwg.org/multipage/input.html#e-mail-state-(type=email)
     // https://github.com/servo/servo/issues/11444
     // https://searchfox.org/mozilla-central/rev/403038737ba75af3842ba6b43b6e2fb47eb06609/dom/html/input/SingleLineTextInputTypes.cpp#192

     let name_field = TextField {
         name: "name".into(),
         value: "".into(),
         maxlength: 12
     };

match serde_json::to_string(&name_field) {
  Ok(j) => {
     form_builder::document.body().append_child(form_builder::document.createTextNode(&j));
  },
  _ => {}
};
     let email_field = AnElement::create("input", hashmap!{
         "value": "Hello Rust!",
         "type": "email"
     }, vec![]);
     let label = AnElement::create("label", hashmap!{}, vec![
         name_field.into(),
         "Email".into(),
         email_field.into()
     ]);
match serde_json::to_string(&label) {
  Ok(j) => {
     form_builder::document.body().append_child(form_builder::document.createTextNode(&j));
  },
  _ => {}
};
     form_builder::document.body().append_child(label.build());
     
}
