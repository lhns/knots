ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.4"

lazy val root = (project in file("."))
  .settings(
    name := "knots",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-tagless-core" % "0.16.3"
    ),
    scalacOptions ++= Seq(
      "-Xkind-projector:underscores",
      "-experimental",
    ),
  )
