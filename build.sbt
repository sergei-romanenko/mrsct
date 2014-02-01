name := "mrsct"

version := "0.1.0"

scalaVersion := "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.3"

libraryDependencies += "com.googlecode" % "kiama_2.9.0-1" % "1.1.0"

libraryDependencies += "org.specs2" %% "specs2" % "2.3.7" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
