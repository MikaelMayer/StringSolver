
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

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3"
  
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.2.3"

// Publish to local Maven repository

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
