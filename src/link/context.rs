use std::collections::HashMap;

pub struct Context<'a, T> {
    sup: Vec<HashMap<&'a str, T>>,
}

impl<'a, T> Context<'a, T> {
    pub fn new() -> Self {
        let sup = vec![HashMap::new()];
        Self { sup }
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.sup.iter().rev().map(|m| m.get(key)).find_map(|t| t)
    }

    pub fn insert(&mut self, key: &'a str, value: T) {
        self.sup.last_mut().unwrap().insert(key, value);
    }

    pub fn new_layer(&mut self) {
        self.sup.push(HashMap::new());
    }

    pub fn pop_layer(&mut self) -> HashMap<&'a str, T> {
        self.sup.pop().unwrap()
    }
}

impl<'a, T> Default for Context<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}
