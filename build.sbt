organization  := "com.softwaremill"

name := "free-tagless-compare"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
