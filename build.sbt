
ThisBuild / organization := "com.github.prinsniels"
ThisBuild / scalaVersion := "3.1.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)


lazy val aoc = project
	.in(file("."))
	.settings()



