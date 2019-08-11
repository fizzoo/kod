use rand::Rng;
use std::cmp::Ordering;
use std::convert::Into;
use std::io;
use std::string::String;

fn read_num<N>(prompt: String) -> N
where
    N: std::str::FromStr,
    <N as std::str::FromStr>::Err: std::fmt::Debug,
{
    println!("{}", prompt);
    loop {
        let mut str = String::new();
        io::stdin()
            .read_line(&mut str)
            .expect("Failed to read string for u32");
        match str.trim().parse() {
            Ok(num) => return num,
            Err(_) => {
                println!("Failed to parse number, please provide a number.");
                continue;
            }
        };
    }
}

fn standard() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1, 101);

    loop {
        let guess: u32 = read_num("Please input your guess.".into());

        println!("You guessed: {}", guess);

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal => {
                println!("You win!");
                break;
            }
        }
    }
}

fn thereverse() {
    let secret_number: i32 = read_num("What's the secret number?".into());

    let mut bot = -1000;
    let mut top = 1001;
    loop {
        if bot > top {
            println!("Cannot solve! Exiting.");
            return;
        }
        let mid = (bot + top) / 2;
        println!("Guessing {}", mid);

        match mid.cmp(&secret_number) {
            Ordering::Less => {
                println!("It was too small!");
                bot = mid + 1;
            }
            Ordering::Greater => {
                println!("It was too big!");
                top = mid - 1;
            }
            Ordering::Equal => {
                println!("It matched! Bot won.");
                break;
            }
        }
    }
}

fn main() {
    thereverse();
}
