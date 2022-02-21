ThisBuild / scalaVersion := "3.1.0"
ThisBuild / organization := "com.example"

lazy val root = (project in file ("."))
  .settings(
    name := "WordleAid",
      libraryDependencies += "org.scalatest" % "scalatest_3" % "3.2.11" % Test
  )