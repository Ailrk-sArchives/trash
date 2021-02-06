use std::collections::HashMap;

pub fn create_from_zip() {
    let teams = vec![String::from("T1"), String::from("T2")];
    let init_scores = vec![10, 50];
    let scores: HashMap<_, _> = teams.iter().zip(init_scores.iter()).collect();
    println!("{:#?}", scores);
}

pub fn insert_m() {
    let mut map = HashMap::new();
    map.insert(String::from("hello"), 10);
    map.insert(String::from("world"), 20);
    println!("{:#?}", map);
}

pub fn insert_or() {
    let text = "The brown quick fox jumps over The lazy dog";
    let mut map = HashMap::new();
    for word in text.split(' ') {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }
    println!("{:#?}", map);
}
