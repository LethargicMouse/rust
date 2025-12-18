use std::collections::HashMap;

pub struct Context<'a, T> {
    sup: HashMap<&'a str, T>,
}

impl<'a, T> Context<'a, T> {
    pub fn new() -> Self {
        let sup = HashMap::new();
        Self { sup }
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.sup.get(key)
    }

    pub fn insert(&mut self, key: &'a str, value: T) {
        self.sup.insert(key, value);
    }
}

impl<'a, T> Default for Context<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}
