import Dependencies._

scalacOptions ++= Seq("-Xfatal-warnings")
fork := true
libraryDependencies += ("org.scala-stm" %% "scala-stm" % "0.8")
libraryDependencies += ("com.lihaoyi" %% "fastparse" % "1.0.0")
lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.calendricalcomputations",
      scalaVersion := "2.12.1",
      version      := "3.1.41-SNAPSHOT"
    )),
    name := "CalendaricalComputations",
    libraryDependencies ++= Seq (
    	scalaTest % Test
	)
  )
