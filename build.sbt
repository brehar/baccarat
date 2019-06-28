name := "baccarat"

version := "0.0.1"

organization := "me.bretthartman"

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:postfixOps"
)

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"
