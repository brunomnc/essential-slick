name := "essential-slick-chapter-02"

version := "3.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick"           % "3.0.0",
  "com.h2database"      % "h2"              % "1.4.185",
  "ch.qos.logback"      % "logback-classic" % "1.1.2"
)

initialCommands in console := """
  |import slick.driver.H2Driver.api._
  |import Example._
  |Example.main(Array())
""".trim.stripMargin