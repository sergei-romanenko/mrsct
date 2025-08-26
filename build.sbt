name := "mrsct"

version := "0.1.0"

scalaVersion := "2.12.20"

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies += "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.5.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.9" % "test"

libraryDependencies += "org.specs2" %% "specs2-junit" % "3.8.9" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")

testOptions in Test += Tests.Argument("-oD")

logBuffered in Test := false

parallelExecution in Test := false
