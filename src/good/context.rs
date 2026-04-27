use std::{collections::HashMap, fmt::Debug};

pub struct Context<'a, T> {
    pub sup: Vec<HashMap<&'a str, T>>,
    pub debug: bool,
}

impl<'a, T: Debug> Context<'a, T> {
    pub fn new(debug: bool) -> Self {
        let sup = vec![HashMap::new()];
        Self { sup, debug }
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.sup.iter().rev().map(|m| m.get(key)).find_map(|t| t)
    }

    pub fn insert(&mut self, key: &'a str, value: T) {
        if self.debug {
            eprintln!("> {key} = {value:?}");
        }
        self.sup.last_mut().unwrap().insert(key, value);
    }

    pub fn new_layer(&mut self) {
        if self.debug {
            eprintln!("> new layer");
        }
        self.sup.push(HashMap::new());
    }

    pub fn pop_layer(&mut self) -> HashMap<&'a str, T> {
        if self.debug {
            eprintln!("> pop layer");
        }
        self.sup.pop().unwrap()
    }
}
