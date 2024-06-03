name := "ScalaProficiency"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

// Adding a setting to run multiple main classes
lazy val root = (project in file("."))
  .settings(
    name := "ScalaProficiency",
    version := "0.1",
    scalaVersion := "2.13.6",
    mainClass in Compile := Some("Main")
  )
