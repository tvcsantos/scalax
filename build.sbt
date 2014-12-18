import AssemblyKeys._

assemblySettings

name := "scalax"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

jarName in assembly := "scalax.jar"