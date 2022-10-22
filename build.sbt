val scala3Version = "3.2.0"

ThisBuild / parallelExecution := false

lazy val root = project
  .in(file("."))
  .settings(
    name := "stupid-compiler",
    version := "0.1.0-SNAPSHOT",
    scalacOptions += "-Ykind-projector:underscores",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.8.0",
        "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
