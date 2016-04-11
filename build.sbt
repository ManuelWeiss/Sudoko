name := "sudoku"


version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "namin.github.com/maven-repository" at "http://namin.github.com/maven-repository/"

libraryDependencies ++= Seq(

  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "junit" % "junit" % "4.11"

)