name := "MonadTransformers Example"

version := "0.0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.1",
  "org.typelevel" %% "cats-free" % "0.8.1"
)

// parallel test execution would confound benchmarking:
parallelExecution in Test := false
logBuffered               := false

