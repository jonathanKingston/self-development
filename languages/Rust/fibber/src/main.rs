// Just a series of fib calculations

fn main() {
    first();
    second();
}

fn first() {
    let n = 187;
    let mut fibs: Vec<i128> = Vec::new();
    for i in 1..n {
        let fib = if i > 2 {
            fibs[i-3].checked_add(fibs[i-2])
        } else {
            Some(1)
        };
        match fib {
            Some(fib) => {
                fibs.push(fib);
                println!("Fib seq {} {}", i, fibs[i-1]);
            },
            None => {
                println!("Fib seq would overflow");
                break;
            }
        }
    }
}

fn second() {
    let n = 187;
    let mut last_fibs: (i128, i128) = (1, 1);
    for i in 1..n {
        let fib = if i > 2 {
            last_fibs.0.checked_add(last_fibs.1)
        } else {
            Some(1)
        };
        match fib {
            Some(fib) => {
                last_fibs.0 = last_fibs.1;
                last_fibs.1 = fib;
                println!("Fib seq {} {}", i, fib);
            },
            None => {
                println!("Fib seq would overflow");
                break;
            }
        }
    }
}
