bird: (good "bird" "-cc" "-lraylib" "-lm")

hash: (good "hash_map" "--" "test.txt")

good file *args: (goodc "run" (file + ".good") args)

goodc *args:
  @cargo run --quiet -- {{args}}

