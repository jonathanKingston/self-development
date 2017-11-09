use std::collections::VecDeque;
use std::error::Error;
use std::net::{TcpListener, TcpStream};
use std::io::{Write,BufRead,BufReader};

enum Message {
    Get,
    Put(String)
}

enum RedishishError {
  Error(String)
}

struct Redishish {
    messages: VecDeque<String>,
    listener: TcpListener
}


impl Redishish {
    fn parse_message(&self, content: String) -> Result<Message,RedishishError> {
       let (verb,body) = content.split_at(3);
       match &verb {
         &"Put" => Ok(Message::Put(String::from(body))),
         &"Get" => Ok(Message::Get),
         _ => Err(RedishishError::Error(String::from("Unparsable message")))
       }
    }

    fn run(&mut self) {
        let incoming = self.listener.incoming();
        for stream in incoming {
            let mut unwrapped_stream = stream.unwrap();
            let mut buffred = BufReader::new(unwrapped_stream.try_clone().unwrap());
            let mut content = String::new();
            buffred.read_line(&mut content);

            println!("a{:?}a", content);

            match self.parse_message(content) {
              Ok(Message::Get) => {
                if let Some(first) = self.messages.pop_front() {
                  unwrapped_stream.write(first.as_bytes());
                } else {
                  unwrapped_stream.write(String::from("Done!").as_bytes());
                }
              },
              Ok(Message::Put(body)) => self.messages.push_back(String::from(body)),
              Err(RedishishError::Error(e)) => {
                unwrapped_stream.write(String::from(e).as_bytes());
              }
            };
        }
    }

    fn new(bind_address: String) -> Self {
      Redishish {
        listener: TcpListener::bind(bind_address).unwrap(),
        messages: VecDeque::default()
      }
    }
}

fn main() {
  setup();
}

fn setup() {
    let mut redishish_server = Redishish::new(String::from("127.0.0.1:8080"));
    redishish_server.run();
}
