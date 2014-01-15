
name := "StringSolver"

version := "1.0"

organization := "ch.epfl.lara"

scalaVersion := "2.10.2"

mainClass := Some("ch.epfl.lara.synthesis.stringsolver.Main")

mainClass in oneJar := Some("ch.epfl.lara.synthesis.stringsolver.Main")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

javacOptions += "-Xlint:unchecked"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies +=  "org.scalatest" % "scalatest_2.10.0-RC5" % "2.0.M5-B1" 
