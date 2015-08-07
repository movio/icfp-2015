scalaVersion := "2.11.6"

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies ++= Seq(
  "net.sf.jgap" % "jgap" % "3.4.4",
  "io.spray" %% "spray-json" % "1.3.3",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
