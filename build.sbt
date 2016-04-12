val commonSettings = Seq(name := "xmless",
  organization := "com.ruiandrebatista.xmless",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  scalacOptions in Compile ++= Seq(
    "-encoding", "UTF-8",
    "-deprecation",
    "-unchecked",
    "-feature",
    "-Xlint",
    "-Ywarn-unused-import",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:higherKinds"),
  resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "export-hook" % "1.1.0",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"))




lazy val core = (project in file("core"))
.settings(commonSettings:_*)
  .settings(
  name := "core",
  moduleName := "xmless-core",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats" % "0.4.1",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.5"))


lazy val generic = (project in file("generic"))
  .settings(commonSettings:_*)
  .settings(
    name := "generic",
    moduleName := "xmless-generic",
    libraryDependencies ++= Seq(
        "com.chuusai" %% "shapeless" % "2.3.0"))
  .dependsOn(core)


lazy val root = (project in file("."))
  .aggregate(core, generic)