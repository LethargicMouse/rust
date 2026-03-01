bird: (good "bird" "-cc" "-lraylib" "-lm")

good file *args:
  cargo run --quiet -- run {{file}}.good {{args}}

goodc *args:
  cargo run --quiet -- {{args}}

