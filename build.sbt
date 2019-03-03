
lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.11.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    libraryDependencies ++= Seq(
      "com.github.spinalhdl" % "spinalhdl-core_2.11" % "1.2.2",
      "com.github.spinalhdl" % "spinalhdl-lib_2.11"  % "1.2.2",
      "org.scalatest" % "scalatest_2.11" % "2.2.1",
      "org.yaml" % "snakeyaml" % "1.8"
    ),
    name := "rt"
  ).dependsOn()
lazy val vexRiscv = RootProject(uri("git://github.com/SpinalHDL/VexRiscv.git#f54865bcb8fc0de6002365a1a7544af06bac575b"))

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.6" % "1.0.2")
fork := true
