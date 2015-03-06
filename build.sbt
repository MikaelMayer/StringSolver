
name := "StringSolver"

version := "1.1"

organization := "ch.epfl.lara"

scalaVersion := "2.10.2"

mainClass in (Compile, run) := Some("ch.epfl.lara.synthesis.stringsolver.Main")

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

publishTo := {
val nexus = "https://oss.sonatype.org/"
if (isSnapshot.value)
  Some("snapshots" at nexus + "content/repositories/snapshots")
else
  Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://jsuereth.com/scala-arm</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:MikaelMayer/StringSolver.git</url>
    <connection>scm:git:git@github.com:MikaelMayer/StringSolver.git</connection>
  </scm>
  <developers>
    <developer>
      <id>MikaelMayer</id>
      <name>Mikael Mayer</name>
      <url>http://www.mikaelmayer.com</url>
    </developer>
  </developers>)
// Publish to local Maven repository

publishMavenStyle := true

//publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

initialCommands in console := "import ch.epfl.lara.synthesis.stringsolver._ ; import ImperativeProgram._ ; import CurrentInstance._ ; import ProgramTypes._; HELP; NEW"
