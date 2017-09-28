
name := "serbitcoin"

version := "0.1"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
	"org.bitcoinj" % "bitcoinj-core" % "0.14.5",
	"org.scalactic" %% "scalactic" % "3.0.1",
	"org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

