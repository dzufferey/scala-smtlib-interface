# scala-smtlib-interface

A scala library to interface with SMT-solver using the SMT-LIB standard (v2.0)
The library has been tested against Z3, CVC4, and dReal.


## Examples

See [`src/test/scala/dzufferey/smtlib/SolverSuite.scala`](src/test/scala/dzufferey/smtlib/SolverSuite.scala) for examples about using the library
and [`src/test/scala/dzufferey/smtlib/FormulaSuite.scala`](src/test/scala/dzufferey/smtlib/FormulaSuite.scala) to write formula with a lightweight syntax.


## Features

* TODO ...


## Compiling

This project requires java 6 and can be build it using [sbt](http://www.scala-sbt.org/).

In a console, execute:
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

The last line is required if you want to use it in some other project.
If you want to use it locally do not add the `resolvers` line but instead run `sbt publishLocal`.

## Alternatives

* The [Scala SMT-LIB](https://github.com/regb/scala-smtlib) project is doing pretty much the same thing.
  It is more mature than this project and has support for more SMT-LIB commands/responses.
  However, we have a few life-simplifying features like typechecking formulas, automatically adding symbol declarations, (trying to) reconstruct models, timeouts, ...
* [ScalaSMT](https://bitbucket.org/franck44/scalasmt) is a similar project that started from Scala SMT-LIB and then evolved into a new library.


## ToDo

* Better support for overloading (Int vs Real)
  Right now, the library complains when it see some Real (it should work though).
* An (optional) internalizer (using WeakHashMaps) to reduce the memory consumption when dealing with large formula

