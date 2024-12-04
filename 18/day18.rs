use std::io::{self};

fn main() {
    let mut input = String::new();

    loop {
        match io::stdin().read_line(&mut input) {
            Ok(0) => break,
            Ok(_) => {
                println!("{}", input.trim());
                input.clear();
            }
            Err(_e) => break
        }
    }
}

