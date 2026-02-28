struct pipe {
  int a, b, c;
};

union maybe$pipe {
  struct {
    char c;
    struct pipe d;
  } a;
  struct {
    char c;
  } b;
};
