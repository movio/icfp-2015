scalaVersion := "2.11.6"

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % "1.3.3",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
