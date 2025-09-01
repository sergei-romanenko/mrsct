scalaVersion := "2.13.16"

name := "mrsct"

version := "0.1.0"

resolvers += Resolver.sonatypeCentralSnapshots

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.18.1" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies += "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.5.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "4.21.0" % "test"

libraryDependencies += "org.specs2" %% "specs2-junit" % "4.21.0" % "test"

Test / scalacOptions ++= Seq("-Yrangepos")

Test / testOptions += Tests.Argument("-oD")

Test / logBuffered := false

Test / parallelExecution := false
