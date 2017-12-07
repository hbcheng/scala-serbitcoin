
name := "serbitcoin"

version := "0.1.1"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
	"org.bitcoinj" % "bitcoinj-core" % "0.14.5",
	"commons-codec" % "commons-codec" % "1.10",
	"org.scalactic" %% "scalactic" % "3.0.3",
	"org.scalatest" %% "scalatest" % "3.0.3" % Test
)

logBuffered in Test := false

