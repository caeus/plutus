import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "plutus",
    libraryDependencies += scalaTest % Test
  )
credentials += Credentials.apply(realm = "Sonatype Nexus Repository Manager",host="oss.sonatype.org",
  userName = "caeus", passwd = "OuS0aZ57I$Eg")
useGpg := true
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/caeus/plutus"),
    "scm:git@github.com:caeus/plutus.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "caeus",
    name  = "Alejandro Navas ",
    email = "camilo.a.navas@gmail.com",
    url   = url("http://medium.com/@caeus")
  )
)
ThisBuild / description := "Principled combinators for string parsing and lexing"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/caeus/plutus"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
