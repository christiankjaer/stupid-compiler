val scala3Version = "3.2.1"

inThisBuild(
  List(
    parallelExecution := false,
    semanticdbEnabled := true, // enable SemanticDB
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "stupid-compiler",
    version := "0.1.0-SNAPSHOT",
    scalacOptions ++= Seq(
      "-Ykind-projector:underscores",
      "-unchecked",
      "-deprecation"
    ),
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-parse" % "0.3.8",
      "com.monovore" %% "decline" % "2.4.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
