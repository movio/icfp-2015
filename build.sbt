scalaVersion := "2.11.6"

name := "icfp2015-movio"

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies ++= Seq(
  "net.sf.jgap" % "jgap" % "3.4.4",
  "io.spray" %% "spray-json" % "1.3.2",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

assemblyJarName in assembly <<= name map { _ + ".jar" }

test in assembly := {}