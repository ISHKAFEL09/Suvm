name := "uvm"

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "%ORGANIZATION%"

val chiselVersion = "3.6.0-RC2"
val treadleVersion = "1.6.0-RC2"
val chiseltestVersion = "0.6.0-RC2"
val firrtlVersion = "1.6.0-RC2"

lazy val root = (project in file("."))
  .settings(
    name := "%NAME%",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      //      "edu.berkeley.cs" %% "chiseltest" % chiseltestVersion,
      "edu.berkeley.cs" %% "treadle" % treadleVersion,
      "edu.berkeley.cs" %% "firrtl" % firrtlVersion,
      "org.scalatest" %% "scalatest" % "3.2.9"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )
