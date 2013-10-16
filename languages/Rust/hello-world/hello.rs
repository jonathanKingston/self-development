fn main () {
  let mut count = 1;
  while count < 10 {
    print_msg(fmt!("Hello world %?", count));
    count += 1;
  }
}

fn print_msg(msg: ~str) {
  println(msg + " and goodbye");
}
