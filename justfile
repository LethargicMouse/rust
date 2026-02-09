run bin="linc":
  cargo run --bin {{bin}} {{if bin == "linc" {"run test.lk"} else {""}}}

