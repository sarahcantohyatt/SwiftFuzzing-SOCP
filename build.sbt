name := "FuzzingSwift"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.4" % "test",
						"org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1")
						
mainClass in assembly := Some("FuzzingSwift.fuzzer.Main")