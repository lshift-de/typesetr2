import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

object BuildSettings {
  val buildSettings = SbtScalariform.scalariformSettings ++ Seq(

    // Metadata
    version := "1.0.0-SNAPSHOT",
    organization := "net.lshift",
    homepage := Some(url("https://github.com/lshift/net.lshift.typesetr/")),
    organizationHomepage := Some(url("http://lshift.net")),
    scmInfo := Some(ScmInfo(
      url("https://github.com/lshift/net.lshift.typesetr.git"),
      "git://github.com/lshift/net.lshift.typesetr.git")),
    pomExtra := (
      <developers>
        <developer>
          <id>hubertp-lshift</id>
          <name>Hubert Plociniczak</name>
          <url>https://github.com/hubertp-lshift</url>
        </developer>
      </developers>),

    // Actual settings
    scalacOptions ++= Seq("-deprecation", "-feature"),
    scalaVersion := "2.11.8",

    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies += {
      if (scalaVersion.value == "2.12.0-SNAPSHOT")
        "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
      else
        "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    },
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test    := formattingPreferences
  )

  def formattingPreferences = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
      .setPreference(RewriteArrowSymbols, false)
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
  }
}


object MyBuild extends Build {
  import BuildSettings._

  val sharedCoreSettings = Seq(
    name := "net.lshift.typesetr",
    autoCompilerPlugins := true
  )

  lazy val core = project.in(file("core"))
    .settings(buildSettings: _*)
    .settings(sharedCoreSettings: _*)
    .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",
      "com.github.scopt" %% "scopt" % "3.5.0",
      "io.circe" %% "circe-core" % "0.4.1",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
      "org.yaml" % "snakeyaml" % "1.17",
      "org.scalaz" %% "scalaz-core" % "7.2.4",
      "commons-io" % "commons-io" % "2.4",
      "org.jbibtex" % "jbibtex" % "1.0.15",
      "com.chuusai" %% "shapeless" % "2.3.1",
      "joda-time" % "joda-time" % "2.9.4"
    )
  )

}
