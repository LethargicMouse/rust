use std::{collections::HashMap, fmt::Debug};

use crate::link::analyse::DEBUG;

pub struct Context<'a, T> {
    pub sup: Vec<HashMap<&'a str, T>>,
}

impl<'a, T: Debug> Context<'a, T> {
    pub fn new() -> Self {
        let sup = vec![HashMap::new()];
        Self { sup }
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.sup.iter().rev().map(|m| m.get(key)).find_map(|t| t)
    }

    pub fn insert(&mut self, key: &'a str, value: T) {
        if DEBUG {
            eprintln!("> {key} = {value:?}");
        }
        self.sup.last_mut().unwrap().insert(key, value);
    }

    pub fn new_layer(&mut self) {
        if DEBUG {
            eprintln!("> new layer");
        }
        self.sup.push(HashMap::new());
    }

    pub fn pop_layer(&mut self) -> HashMap<&'a str, T> {
        if DEBUG {
            eprintln!("> pop layer");
        }
        self.sup.pop().unwrap()
    }
}

impl<'a, T: Debug> Default for Context<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}
