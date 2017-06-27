name := "packrat"

version := "0.1"

scalaVersion := "2.12.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.2"

scalacOptions += "-deprecation"
scalacOptions += "-Xfatal-warnings"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
