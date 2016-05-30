
name := "highperfscala"
organization := "highperfscala"

val scalazVersion = "7.2.0"
val specsVersion = "3.7.3"

val slf4s = "org.slf4s" %% "slf4s-api" % "1.7.12"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.0"
val hdrHistogram = "org.mpierce.metrics.reservoir" %
  "hdrhistogram-metrics-reservoir" % "1.1.0"
val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersion
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % scalazVersion
val specs2 = "org.specs2" %% "specs2-core" % specsVersion
val specs2ScalaCheck = "org.specs2" %% "specs2-scalacheck" % specsVersion
val joda = "joda-time" % "joda-time" % "2.8.2"
val jodaConvert = "org.joda" % "joda-convert" % "1.8"
val breeze = "org.scalanlp" %% "breeze" % "0.11.2" exclude(
  "org.spire-math", "spire_2.11")
val spire = "org.spire-math" %% "spire" % "0.11.0"
val spireMacro = "org.spire-math" %% "spire-macros" % "0.11.0"
val saddle = "org.scala-saddle" %% "saddle-core" % "1.3.4"


val baseOptions = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion := "2.11.8",
  fork := true,
  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  libraryDependencies ++= List(
    slf4s, scalaCheck, hdrHistogram, scalaz, joda, jodaConvert,
    specs2 % "test", specs2ScalaCheck % "test"
  )
)

lazy val root = Project(
  id = "highperfscala",
  base = file("."),
  settings = baseOptions ++ Seq(
    onLoadMessage ~= (_ + (if (sys.props("java.specification.version") != "1.8") {
      """
        |You seem to not be running Java 1.8.
        |While the provided code may still work, we recommend that you
        |upgrade your version of Java.
      """.stripMargin
    } else "")))) aggregate(
  chapter2, chapter3, chapter4, chapter5, chapter6, chapter7)

lazy val chapter2 = Project(
  id = "chapter2",
  base = file("chapter2"),
  settings = baseOptions
).enablePlugins(JmhPlugin)

lazy val chapter3 = Project(
  id = "chapter3",
  base = file("chapter3"),
  settings = baseOptions
).enablePlugins(JmhPlugin)

lazy val chapter4 = Project(
  id = "chapter4",
  base = file("chapter4"),
  settings = baseOptions
).settings(
  libraryDependencies ++= Seq(breeze, spire, spireMacro, saddle)
).enablePlugins(JmhPlugin)

lazy val chapter5 = Project(
  id = "chapter5",
  base = file("chapter5"),
  settings = baseOptions
).enablePlugins(JmhPlugin)

lazy val chapter6 = Project(
  id = "chapter6",
  base = file("chapter6"),
  settings = baseOptions
).settings(
  libraryDependencies ++= Seq(scalazConcurrent)
).enablePlugins(JmhPlugin)

lazy val chapter7 = Project(
  id = "chapter7",
  base = file("chapter7"),
  settings = baseOptions
).settings(
  libraryDependencies ++= Seq(scalazConcurrent)
).enablePlugins(JmhPlugin)
