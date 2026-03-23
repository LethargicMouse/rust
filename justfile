hash: (good "hash_map" "--" "test.txt")

bird: (good "bird" "-cc" "-lraylib" "-lm")

good file *args: (goodc "run" (file + ".good") args)

goodc *args:
  @cargo run --quiet -- {{args}}

