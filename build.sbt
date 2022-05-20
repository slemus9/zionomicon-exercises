ThisBuild / scalaVersion := "3.1.2"

val zioVersion = "2.0.0-RC6"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion,
  "dev.zio" %% "zio-test-sbt" % zioVersion
)

scalacOptions ++= Seq("-new-syntax", "-rewrite")