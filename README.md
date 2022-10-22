# Stupid compiler

- Zig runtime
- Compiler written in Scala targeting x86-64 for now


For the stupid program in `examples/add.stupid`
```
fun add(x, y) = if is_zero(y) then x else add(x+1, y-1)
in add(42, 1337)
```

You can compile it with sbt (requires zig on the path)

```
$ sbt run "examples examples/add.stupid add"
$ ./add
1379
```
