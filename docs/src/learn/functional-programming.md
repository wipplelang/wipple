# Functional programming

Work in progress! In the meantime:

- You can use `.` to chain function calls together. For example, these two lines of code are the same:

  ```wipple
  a . b . c
  (c (b a))
  ```

- You can use `|` to pipe functions. For example, these two lines of code are the same:

  ```wipple
  a | b | c
  x -> c (b (a x))
  ```
