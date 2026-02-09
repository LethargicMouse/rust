run bin="linc":
  cargo run --quiet --bin {{bin}} {{if bin == "linc" {"run test.lk"} else {""}}}

