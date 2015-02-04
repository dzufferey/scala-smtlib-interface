# scala-smtlib-interface

A scala library to interface with SMT-solver using the SMT-LIB standard (v2.0)
The library has been tested against Z3, CVC4, and dReal.


## Examples

See `src/test/scala/dzufferey/smtlib/SolverSuite.scala` for some example on how to use the library.


## Features

* TODO ...


## Compiling

This project requires java 6 and can be build it using [sbt](http://www.scala-sbt.org/).
To install sbt follow the instructions at [http://www.scala-sbt.org/release/tutorial/Setup.html](http://www.scala-sbt.org/release/tutorial/Setup.html).

Then, in a console, execute:
```
$ sbt
> compile
```

## Using it

To use it in your projects your need to add the following two lines in your `build.sbt`:
```scala
resolvers +=  "dzufferey maven repo" at "https://github.com/dzufferey/my_mvn_repo/raw/master/repository"

libraryDependencies += "io.github.dzufferey" %% "scala-smtlib-interface" % "0.1-SNAPSHOT"
```

The last line is requried if you want to use it in some other project.
If you want to use it locally do not add the `resolvers` line but instead run `sbt publishLocal`.

## Alternatives

The [scala-smtlib](https://github.com/regb/scala-smtlib) project is doing pretty much the same thing.
It is more mature than this project and has support for more SMT-LIB commands/responses.
However, we have a few life-simplifying features like typechecking formulas, automatically adding symbol declarations, (trying to) reconstruct models, timeouts, ...

## TODO

* better support for overloading (Int vs Real)
* better support for variadic functions (and, or, ...) from the typing perspective
Right now, the library will complain when it see some Real and variadic functions (it should work though).
