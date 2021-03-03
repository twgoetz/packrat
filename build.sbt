name := "packrat"

version := "0.1"

scalaVersion := "2.13.2"

sbtVersion := "1.0.4"

scalacOptions += "-deprecation"
scalacOptions += "-Xfatal-warnings"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
