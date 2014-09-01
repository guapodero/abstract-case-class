organization := "org.dbaumann"

name := "abstract-case-class"

version := "0.1"

scalaVersion := "2.11.1"

crossScalaVersions := Seq("2.10.4", "2.11.1")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies ++= (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % "2.0.1")
  else Nil
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.13" % Test
)

unmanagedSourceDirectories in Compile <+= (sourceDirectory in Compile, scalaBinaryVersion){
  (sourceDir, version) => sourceDir / (if (version.startsWith("2.10")) "scala_2.10" else "scala_2.11")
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

// support cross compilation introduces several deprecation warnings which can't be suppressed individually
// see https://issues.scala-lang.org/browse/SI-1781
scalacOptions in ThisBuild ++= Seq("-unchecked"/*, "-deprecation"*/)

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org"
  if (isSnapshot.value) Some("snapshots" at s"$nexus/content/repositories/snapshots")
  else Some("releases" at s"$nexus/service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/dbaumann/abstract-case-class</url>
  <licenses>
    <license>
      <name>MIT</name>
      <url>http://opensource.org/licenses/MIT</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:dbaumann/abstract-case-class.git</url>
    <connection>scm:git:git@github.com:dbaumann/abstract-case-class.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dbaumann</id>
      <name>Dan Baumann</name>
      <url>https://github.com/dbaumann</url>
    </developer>
  </developers>)

useGpg := true
