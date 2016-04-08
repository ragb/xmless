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
    "-language:postfixOps"))


lazy val root = (project in file("."))
.settings(commonSettings:_*)
  .settings(
  name := "xmless",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats" % "0.4.0",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
        "com.chuusai" %% "shapeless" % "2.3.0",
"org.scalatest" %% "scalatest" % "2.2.4" % "test"))

