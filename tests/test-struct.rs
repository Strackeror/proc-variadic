use proc_variadic::variadic;

variadic!(struct Test<...T> {
  tuple: (...T)
});
