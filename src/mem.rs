use std::mem::take;

pub fn update<T: Default>(t_ref: &mut T, f: impl FnOnce(T) -> T) {
    let t = take(t_ref);
    *t_ref = f(t);
}

pub fn map_box<T: Default>(mut b: Box<T>, f: impl FnOnce(T) -> T) -> Box<T> {
    update(b.as_mut(), f);
    b
}
