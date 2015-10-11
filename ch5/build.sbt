
initialCommands in console := """
    |import ch5._
    |""".stripMargin

lazy val root = (project in file(".")).
	settings(
		name:="hello",
		version:="1.0",
		scalaVersion:="2.11.4"
		)