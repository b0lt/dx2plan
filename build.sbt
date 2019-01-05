enablePlugins(WorkbenchPlugin)

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

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
  ).jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.6",
    ),
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
