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
$ sbt "run examples/add.stupid add"
$ ./add
1379
```

## How to make the CI happy

Run scalafix, scalafmt and the test suite

```
sbt:stupid-compiler> scalafmtAll
sbt:stupid-compiler> scalafixAll
sbt:stupid-compiler> test
```

## Next steps
- Explore various optimizations
    * Constant folding
    * Peephole optimizations
    * Tail call optimization
    * Inlining
- Heap allocations
- Closures and lambdas
- Type system
- Algebraic data types and pattern matching
