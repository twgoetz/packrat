name := "packrat"

version := "0.1"

scalaVersion := "2.12.2"

sbtVersion := "1.0.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.2"
libraryDependencies += "com.geirsson" %% "scalafmt-core" % "1.1.0"

scalacOptions += "-deprecation"
scalacOptions += "-Xfatal-warnings"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
