val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "inc",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.8.0",
        "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
