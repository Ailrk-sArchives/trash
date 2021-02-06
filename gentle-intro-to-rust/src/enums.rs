pub enum IpAddr {
    V4(u8, u8, u8, u8),
    V6(String),
}

pub fn ipaddress() -> (IpAddr, IpAddr) {
    let home = IpAddr::V4(127, 0, 0, 1);
    let loopback = IpAddr::V6(String::from("::1"));
    (loopback, home)
}

pub enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    pub fn call(&self) {
        match self {
            Message::Quit => println!("Quit"),
            Message::Move { x, y } => println!("{} {}", x, y),
            Message::Write(s) => println!("{}", s),
            Message::ChangeColor(r, g, b) => println!("{}{}{}", r, g, b),
        }
    }
}
