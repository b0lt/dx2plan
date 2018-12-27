enablePlugins(ScalaJSPlugin, WorkbenchPlugin)

scalaVersion := "2.12.8"

name := "dx2plan"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.6",
  "com.lihaoyi" %%% "scalatags" % "0.6.7",
  "com.lihaoyi" %%% "scalarx" % "0.4.0",
  "com.nrinaudo" %%% "kantan.csv" % "0.5.0",
  "com.nrinaudo" %%% "kantan.csv-generic" % "0.5.0"
)

sourceGenerators in Compile += Def.task {
  val demonsFile = (resourceDirectory in Compile).value / "demons.csv"
  val skillsFile = (resourceDirectory in Compile).value / "skills.csv"

  val sourceDir = (sourceManaged in Compile).value
  val sourceFile = sourceDir / "Dx2Db.scala"

  // Java has a 64k limit on the length of string literals, so chunk the file up and concatenate.
  // TODO: Convert to json and load it directly instead of this abomination.
  val demonChunks = IO.read(demonsFile).grouped(60000).toList.map(_.replaceAllLiterally("$", "$$"))
  val skillChunks = IO.read(skillsFile).grouped(60000).toList.map(_.replaceAllLiterally("$", "$$"))

  val scalaCode =
    s"""
    package dx2plan

    object Dx2Db {
      final val demons = ${demonChunks.map("raw\"\"\"" + _ + "\"\"\"").mkString(" + ")}
      final val skills = ${skillChunks.map("raw\"\"\"" + _ + "\"\"\"").mkString(" + ")}
    }
    """
  IO.write(sourceFile, scalaCode)
  Seq(sourceFile)
}.taskValue
