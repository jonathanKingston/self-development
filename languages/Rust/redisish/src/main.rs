extern crate futures;
extern crate futures_cpupool;

use std::sync::{Arc, Mutex};
use std::collections::VecDeque;
use std::error::Error;
use std::net::{TcpListener, TcpStream, Shutdown};
use std::io::{Write,BufRead,BufReader};
use std::io::Error as IoError;
use futures::future::{Future, join_all};
use futures_cpupool::Builder;


enum Message {
    Get,
    Put(String)
}

#[derive(Debug)]
enum RedisishError {
    Error(String),
    Io(IoError)
}

impl From <IoError> for RedisishError {
  fn from(er: IoError) -> Self {
    RedisishError::Io(er)
  }
}

struct Redisish {
    messages: Mutex<VecDeque<String>>,
}

impl Redisish {
    fn get(&self) -> Option<String> {
        let mut messages = self.messages.lock().unwrap();
        if let Some(first) = messages.pop_front() {
          Some(first)
        } else {
          None
        }
    }

    fn put(&self, message: String) {
        let mut messages = self.messages.lock().unwrap();
        messages.push_back(String::from(message))
    }

    fn new() -> Self {
      Redisish {
        messages: Mutex::new(VecDeque::default())
      }
    }
}




#[derive(Debug)]
enum TcpServerError {
    Error(String),
    Io(IoError)
}

impl From <IoError> for TcpServerError {
  fn from(er: IoError) -> Self {
    TcpServerError::Io(er)
  }
}

struct TcpServer {
    listener: TcpListener,
    redisish: Redisish
}

impl TcpServer {
    fn parse_message(&self, content: String) -> Result<Message,TcpServerError> {
       let (verb,body) = content.split_at(3);
       match &verb {
         &"Put" => Ok(Message::Put(String::from(body))),
         &"Get" => Ok(Message::Get),
         _ => Err(TcpServerError::Error(String::from("Unparsable message")))
       }
    }

    fn stream_run(&self, stream: &mut TcpStream) {
        let mut buffred = BufReader::new(stream.try_clone().unwrap());
        let mut content = String::new();
        buffred.read_line(&mut content);

        println!("read");
        match self.parse_message(content) {
          Ok(Message::Get) => {
            if let Some(first) = self.redisish.get() {
              stream.write(first.as_bytes());
            } else {
              stream.write("Done!".as_bytes());
            }
          },
          Ok(Message::Put(body)) => self.redisish.put(String::from(body)),
          Err(e) => {
            write!(stream, "{:?}", e);
          }
        };
    }

    fn run(&mut self) -> Result<(),TcpServerError> {
        let cpu_pool = Builder::new().create();
        loop {
/*
            if stream.is_err() 
              break;
            }
*/
          //  cpu_pool.spawn_fn(move || {
                let (mut stream, _) = self.listener.accept().unwrap();
                let result: Result<_, ()> = Ok(self.stream_run(&mut stream));
                //let result: Result<_, ()> = Ok(1);
            //stream.shutdown(Shutdown::Both);

           //     result
          //  });

            //self.stream_run(&mut stream.unwrap());
        }
        //join_all(vec![x].into_iter());
        Ok(())
    }

    fn new(listener: TcpListener, redisish: Redisish) -> Self {
      TcpServer {
        redisish: redisish,
        listener: listener
      }
    }

    fn bind<S>(bind_address: S, redisish: Redisish) -> Result<Self,TcpServerError>
    where S: AsRef<str> {
      let listener = TcpListener::bind(bind_address.as_ref())?;
      Ok(TcpServer::new(listener, redisish))
    }
}

#[cfg(test)]
mod tests {
    use super::{Redisish,RedisishError};

    #[test]
    fn does_redisish_run() {
        assert!(Redisish::bind("barf").is_err());
        assert!(Redisish::bind("127.0.0.1:8080").is_ok());
        assert!(Redisish::bind(String::from("127.0.0.1:8080")).is_ok());
    }
}

fn main() {
    let mut redisish_result = Redisish::new();
    let mut tcp_server_result = TcpServer::bind("127.0.0.1:8080", redisish_result);
    if let Ok(mut tcp_server) = tcp_server_result {
        tcp_server.run();
    }
}
