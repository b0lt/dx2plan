enablePlugins(WorkbenchPlugin)

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

scalaVersion := "2.12.8"

lazy val dx2plan = crossProject(JSPlatform).withoutSuffixFor(JSPlatform).crossType(CrossType.Pure)
  .in(file("dx2plan"))
  .settings(
    name := "dx2plan",
    version := "0.1-SNAPSHOT",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % "0.7.1",
      "com.nrinaudo" %%% "kantan.csv" % "0.5.0",
      "com.nrinaudo" %%% "kantan.csv-generic" % "0.5.0",
      "org.scala-js" %%% "scalajs-dom" % "0.9.6",
      "com.lihaoyi" %%% "scalatags" % "0.6.7",
      "com.lihaoyi" %%% "scalarx" % "0.4.0",
    ),
    sourceGenerators in Compile += Def.task {
      val dx2dbPath = baseDirectory.value / "../../dx2db/shared/src/main/resources/dx2db.json"
      val sourceDir = (sourceManaged in Compile).value
      val sourceFile = sourceDir / "Dx2DbBlob.scala"

      // Java has a 64k limit on the length of string literals, so chunk the file up and concatenate.
      // TODO: Load the json directly instead of this abomination.
      val chunks = IO.read(dx2dbPath).grouped(60000).toList.map(_.replaceAllLiterally("$", "$$"))

      val scalaCode =
        s"""
        package dx2plan

        object Dx2DbBlob {
          final val blob = ${chunks.map("raw\"\"\"" + _ + "\"\"\"").mkString(" + ")}
        }
        """
      IO.write(sourceFile, scalaCode)
      Seq(sourceFile)
    }.taskValue,
  ).dependsOn(dx2db)

lazy val dx2db = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("dx2db"))
  .settings(
    name := "dx2db",
    version := "0.1-SNAPSHOT",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % "0.7.1",
      "com.nrinaudo" %%% "kantan.csv" % "0.5.0",
      "com.nrinaudo" %%% "kantan.csv-generic" % "0.5.0",
    ),
    unmanagedResourceDirectories in Compile += baseDirectory.value / "../shared/src/main/resources",
    fork in run := true,
    baseDirectory in run := baseDirectory.value / "../../dx2db/shared/src/main/resources",
  )

lazy val altemascraper = crossProject(JVMPlatform).withoutSuffixFor(JVMPlatform).crossType(CrossType.Pure)
  .in(file("altemascraper"))
  .settings(
    name := "altemascraper",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % "0.7.1",
      "net.ruippeixotog" %%% "scala-scraper" % "2.1.0",
    ),
    fork in run := true,
    baseDirectory in run := baseDirectory.value / "../../dx2db/shared/src/main/resources",
  )
