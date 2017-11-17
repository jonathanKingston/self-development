use std::process::Command;
use std::{thread, time};
use std::str::FromStr;
use std::num::ParseIntError;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut router_ip = String::from("192.168.2.1");
    if args.len() > 1 {
        router_ip = args[1].clone();
    }
    let mut interface = String::from("wlp2s0");
    if args.len() > 2 {
        interface = args[2].clone();
    }
    let mut network_checker = NetworkChecker::new(router_ip, interface);
    network_checker.check_loop();
//    network_checker.reset_network();
}

enum NetworkCheckerResponse {
    Res(&'static str),
    ResInt(usize),
}
enum NetworkCheckerError {
    StrError(&'static str),
    IntError(ParseIntError)
}

impl From <ParseIntError> for NetworkCheckerError {
  fn from(er: ParseIntError) -> Self {
    NetworkCheckerError::IntError(er)
  }
}

struct NetworkChecker {
    byte_count: usize,
    reset_count: u32,
    debug: u8,
    router_ip: String,
    interface: String,
}

impl NetworkChecker {
    fn new(router_ip: String, interface: String) -> Self {
      NetworkChecker {
          debug: 0,
          byte_count: 0,
          reset_count: 0,
          router_ip: router_ip,
          interface: interface
      }
    }

    fn debug(&self, level: u8, message: &str) {
        if self.debug < level { println!("{}", message); }
    }

    fn ping_network(&mut self) -> Result<NetworkCheckerResponse, NetworkCheckerError> {
        self.debug(2, "Ping to pass data back and increment byte count.");
        let output = self.run_command("ping", &[
            &self.router_ip,
            "-t 1",
           // "-w 250",
            "-c 1",
        ])?;
        if !output.status.success() {
           return Err(NetworkCheckerError::StrError("Err"));
        }
        Ok(NetworkCheckerResponse::Res("Ok"))
    }

    fn check_ping_loop(&mut self) -> Result<NetworkCheckerResponse, NetworkCheckerError> {
        let mut error_count = 0;
        let pause = 400;
        loop {
            let ping_response = self.ping_network();
            if let Ok(_) = ping_response {
                return self.check_loop();
            }
            error_count += 1;
            if error_count > 2 {
                return self.reset_network();
            }
            if self.debug > 2 {
                println!("Error ping count {}", error_count);
            }
            thread::sleep(time::Duration::from_millis(pause));
        }
    }

    fn check_loop(&mut self) -> Result<NetworkCheckerResponse, NetworkCheckerError> {
        let mut error_count = 0;
        let pause = 100;
        loop {
            let current_byte_count = self.byte_count;
            if let Ok(NetworkCheckerResponse::ResInt(new_byte_count)) = self.check_byte_count() {
                if current_byte_count < new_byte_count {
                    self.byte_count = new_byte_count;
                    error_count = 0;
                } else {
                    error_count += 1;
                }
            } else {
                error_count += 1;
            }
            if error_count > 4 {
                return self.check_ping_loop();
            }
            if self.debug > 1 {
                println!("Error count {}", error_count);
            }
            thread::sleep(time::Duration::from_millis(pause));
        }
    }

    fn run_command(&self, command: &str, args: &[&str]) -> Result<std::process::Output, NetworkCheckerError> {
        if self.debug > 1 {
            println!("Command: {} {:?}", command, args);
        }
        let output = Command::new(command)
                             .args(args)
                             .output();
        if let Ok(output_response) = output {
            if self.debug > 1 {
              println!("status: {}", output_response.status);
              println!("stdout: {}", String::from_utf8_lossy(&output_response.stdout));
              println!("stderr: {}", String::from_utf8_lossy(&output_response.stderr));
            }
            Ok(output_response)
        } else if let Err(output_error) = output {
            if self.debug > 1 {
                println!("Err {:?}", output_error);
            }
            Err(NetworkCheckerError::StrError("Error happened in running command"))
        } else {
            Err(NetworkCheckerError::StrError("Unknown Error"))
        }
    }

    fn check_byte_count(&self) -> Result<NetworkCheckerResponse, NetworkCheckerError> {
        let output = self.run_command("ifconfig", &[
            &self.interface
        ])?;
        let string_output = String::from_utf8_lossy(&output.stdout);
        let mut parts = string_output.split("RX packets ");
        if let Some(second_part) = parts.nth(1) {
            let mut parts2 = second_part.split("bytes");
            if let Some(parties2) = parts2.nth(0) {
                let bytes = usize::from_str(parties2.trim())?;
                if self.debug > 1 {
                    println!("Bytes: {:?}", bytes);
                }
                return Ok(NetworkCheckerResponse::ResInt(bytes));
            }
        }
        return Err(NetworkCheckerError::StrError("blah"));
    }

    fn reset_network(&mut self) -> Result<NetworkCheckerResponse, NetworkCheckerError> {
        self.reset_count += 1;
        self.byte_count = 0;
        if self.debug > 1 {
            println!("Resetting network {}", self.reset_count);
        }
        let output = self.run_command("service", &[
            "network-manager",
            "restart",
        ])?;
        self.debug(2, "Waiting for network to come back");
        let output = self.run_command("nm-online", &[]);
        if let Ok(output) = output {
            self.check_loop();
            return Ok(NetworkCheckerResponse::Res("Reset network"));
        }
        return self.reset_network();
    }
}
