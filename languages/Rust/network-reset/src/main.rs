use std::process::Command;
use std::{thread, time};
use std::process::Output;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    //check_loop();
    let mut router_ip = String::from("192.168.2.1");
    if args.len() > 2 {
        router_ip = args[1].clone();
    }
    let mut interface = String::from("wlp2s0");
    if args.len() > 2 {
        interface = args[2].clone();
    }
    let mut network_checker = NetworkChecker::new(router_ip, interface);
    network_checker.check_loop();
}

enum NetworkCheckerResponse {
    Res(&'static str),
    Output,
}

struct NetworkChecker {
    reset_count: i32,
    router_ip: String,
    interface: String,
}

impl NetworkChecker {
    fn new(router_ip: String, interface: String) -> Self {
      NetworkChecker {
          reset_count: 0,
          router_ip: router_ip,
          interface: interface
      }
    }

    fn check_loop(&mut self) -> Result<NetworkCheckerResponse, std::io::Error> {
        let mut error_count = 0;
        loop {
            let mut pause = 1000;
            let output = self.run_command("ping", &[
                &self.router_ip,
                "-t 1",
               // "-w 250",
                "-c 1",
            ])?;
            
            if output.status.success() {
                error_count = 0;
            } else {
                error_count += 1;
                if error_count > 2 {
                    return self.reset_network();
                }
                pause = 100;
            }
            println!("Error count {}", error_count);
            thread::sleep(time::Duration::from_millis(pause));
        }
    }

    fn run_command(&self, command: &str, args: &[&str]) -> Result<std::process::Output, std::io::Error> {
        println!("Command: {} {:?}", command, args);
        let output = Command::new(command)
                             .args(args)
                             .output()?;

        println!("status: {}", output.status);
        println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
        println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        Ok(output)
    }

    fn reset_network(&mut self) -> Result<NetworkCheckerResponse, std::io::Error> {
        self.reset_count += 1;
        println!("Resetting network {}", self.reset_count);
        let output = self.run_command("service", &[
            "network-manager",
            "restart",
        ])?;
        let mut loop_count = 0;
        loop {
            println!("Waiting for network to come back: {}", loop_count);
            let output = self.run_command("ifconfig", &[
                &self.interface
            ]);
            let output_result = output.unwrap();
            let string_output = String::from_utf8_lossy(&output_result.stdout);
            let mut lines = string_output.split("\n");
            if let Some(second_line) = lines.nth(1) {
                if second_line.trim().starts_with("inet") {
                    self.check_loop();
                    return Ok(NetworkCheckerResponse::Res("Reset network"));
                }
            }
            loop_count += 1;
            if loop_count > 150 {
                return self.reset_network();
            }
            thread::sleep(time::Duration::from_millis(100));
        }
    }
}
