import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.calendricalcomputations",
      scalaVersion := "2.12.1",
      version      := "3.1.4-SNAPSHOT"
    )),
    name := "CalendaricalComputations",
    libraryDependencies ++= Seq (
    	scalaTest % Test
	)
  )
