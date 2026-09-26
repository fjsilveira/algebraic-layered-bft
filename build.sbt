val scala3Version = "3.8.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "decoupling-bft-consensus",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all"),
    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-bits" % "1.2.1",
      "dev.zio" %% "zio" % "2.1.26",
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
      "org.scalacheck" %% "scalacheck" % "1.18.1" % Test
    ),
    run / fork := true
  )
