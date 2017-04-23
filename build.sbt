name := "packrat"

version := "0.1"

scalaVersion := "2.11.8"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.9"

scalacOptions += "-deprecation"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
