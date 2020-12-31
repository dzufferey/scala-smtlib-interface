name := "scala-smtlib-interface"

organization := "com.github.dzufferey"

version := "1.0.0"

scalaVersion := "2.13.4"

crossScalaVersions := Seq("2.12.12", "2.13.4")

scalacOptions in Compile ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature"
)

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "io.github.dzufferey" %% "misc-scala-utils" % "1.0.0"
)
