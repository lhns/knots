ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.9.0"

lazy val root = (project in file("."))
  .settings(
    name := "knots",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.scalameta" %% "munit" % "1.3.6" % Test
    ),
    // -Yretain-trees: spike C checks the bodies of target trait members (must be `= inherited`).
    scalacOptions ++= Seq("-Xcheck-macros", "-Yretain-trees"),
    // Cell uses java.lang.ScopedValue (final since JDK 25).
    initialize := {
      initialize.value
      val version = sys.props("java.specification.version").toInt
      require(version >= 25, s"needs JDK 25+ for ScopedValue, running on $version")
    }
  )
