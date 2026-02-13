run bin="linc" *args:
  cargo run --quiet --bin {{bin}} {{if bin == "linc" {"run test.lk -cc -lraylib -lm"} else {""}}} {{args}}

