name := "protein-refinery"

version := "0.2-SNAPSHOT"

scalaVersion := "2.12.4"

resolvers += Resolver.sonatypeRepo("snapshots")

autoCompilerPlugins := true
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.5" cross CrossVersion.binary)
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

scalastyleFailOnError := true

val commonScalacOptions = List(
  "-language:higherKinds",
  "-Xlint",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ypatmat-exhaust-depth", "40",
  "-Xfuture")

scalacOptions ++= "-Ywarn-unused-import" :: commonScalacOptions
scalacOptions in (Compile, console) := commonScalacOptions
scalacOptions in (Test, console) := commonScalacOptions

libraryDependencies ++= Seq(
  "com.github.tomasmikula" %% "nutcracker" % "0.2-SNAPSHOT",
  "org.typelevel" %% "algebra" % "0.6.0",
  "org.scalaz" %% "scalaz-core" % "7.3.0-M18",
  "org.scalacheck" %% "scalacheck" % "1.13.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF") // show full stack traces

fork := true


// Config for refinery REPL session.
// Extends Test in order to include compiled sources on classpath.
val ReplSession = config("repl-session") extend(Test)

val root = project.in(file("."))
  .configs(ReplSession)

// define task that starts the refinery REPL session
lazy val session = TaskKey[Unit]("session")
session := (Seq(
  console in (root, ReplSession)
).dependOn).value

// session initialization
initialCommands in (Test, console) := """
  | val $session = proteinrefinery.newReplSession()
  | import $session._
  | import $session.lib._
  |
""".stripMargin
