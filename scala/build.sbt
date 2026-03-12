name         := "microtortoise"
organization := "org.nlogo"
licenses     += ("Creative Commons Zero v1.0 Universal Public Domain Dedication", url("https://creativecommons.org/publicdomain/zero/1.0/"))
version      := "0.1.0"
isSnapshot   := true

scalaVersion          := "3.7.0"
Compile / scalaSource := baseDirectory.value / "src" / "main"
Test / scalaSource    := baseDirectory.value / "src" / "test"
scalacOptions        ++= Seq("-deprecation", "-unchecked", "-Xfatal-warnings", "-encoding", "us-ascii", "-release", "17")

resolvers           += "Cloudsmith" at "https://dl.cloudsmith.io/public/netlogo/netlogo/maven/"
libraryDependencies ++=
  Seq( "org.nlogo" % "netlogo" % "7.0.3"
     , "org.jogamp.jogl" % "jogl-all" % "2.4.0" from "https://s3.amazonaws.com/ccl-artifacts/jogl-all-2.4.0.jar"
     , "org.jogamp.gluegen" % "gluegen-rt" % "2.4.0" from "https://s3.amazonaws.com/ccl-artifacts/gluegen-rt-2.4.0.jar"
     )
