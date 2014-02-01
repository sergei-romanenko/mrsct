name := "mrsct"

version := "0.1.0"

scalaVersion := "2.9.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.9.1"

libraryDependencies += "com.googlecode" % "kiama_2.9.0-1" % "1.1.0"

libraryDependencies += "org.specs2" %% "specs2" % "1.5"

scalacOptions ++= Seq("-unchecked", "-deprecation")
