name := "uvm"

ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "%ORGANIZATION%"

val spinalVersion = "1.9.0"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
val scalatest = "org.scalatest" %% "scalatest" % "3.2.15"
val chisel3 = "edu.berkeley.cs" %% "chisel3" % "3.6.0-RC2"

lazy val projectname = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      spinalCore, spinalLib, spinalIdslPlugin, scalatest, chisel3
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-language:implicitConversions",
      "-language:postfixOps",
    ),
  )
