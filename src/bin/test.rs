fn main() {
    test()
}

fn test() {
    test2();
    unreachable!();
}

fn test2() {
    unreachable!();
}
