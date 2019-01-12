name := "fpinscala"

version := "0.1"

scalaVersion := "2.12.8"

lazy val root = Project(id="fpinscala", base = file("."))

enablePlugins(JmhPlugin)