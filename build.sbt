val dottyVersion = "0.24.0-RC1"
// val scalaVersion = "2.13.0"
lazy val root = project
  .in(file("."))
  .settings(
    name := "free-monad-talk",
    version := "0.1.0",

    scalaVersion := "2.13.3",
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1",
    libraryDependencies += "org.typelevel" %% "cats-free" % "2.1.1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.4",
    scalafmtOnCompile := true
  )
