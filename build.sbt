import org.typelevel.sbt.tpolecat.*

ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "3.4.0"

lazy val root = (project in file(".")).settings(
  name := "typechecking-playground",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.5.3",
    "org.typelevel" %% "cats-effect-kernel" % "3.5.3",
    "org.typelevel" %% "cats-parse" % "1.1.0",
    "org.typelevel" %% "cats-effect-std" % "3.5.3",
    "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test)
)
