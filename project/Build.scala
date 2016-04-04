import sbt.Keys._
import sbt._

object ScalaDroolsBuild extends Build {
  import Deps._
  import Reps._

  val buildOrganization = "org.hfgiii.drools"
  val buildVersion = "0.1.0-SNAPSHOT"
  val buildPublishTo = None
  val buildScalaVersion = "2.11.8"

  val buildParentName = "parent"
  val coreName = "core"

  val BaseSettings = Project.defaultSettings ++ Seq(
    organization := buildOrganization,
    publishTo := buildPublishTo,
    scalaVersion := buildScalaVersion,
    crossScalaVersions := Seq("2.10.8", "2.11.8"),
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    resolvers := reps)

  def scalaDroolsProject(projectName: String): Project = {
    Project(
      id = projectName,
      base = file(projectName),
      settings = BaseSettings ++ Seq(
        name := projectName,
        version := buildVersion))
  }

  lazy val root = Project(
    id = buildParentName,
    base = file("."),
    settings = BaseSettings) aggregate coreProject


  lazy val coreProject =
    scalaDroolsProject(coreName).
      settings(libraryDependencies := deps ++  testingDeps)

}

object Reps {
  val reps = Seq(
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    "gphat" at "https://raw.github.com/gphat/mvn-repo/master/releases/")
}

object Deps {

  val logDeps = List(
    "org.slf4j" % "jcl-over-slf4j" % "1.7.7",
    "org.slf4j" % "slf4j-api" % "1.7.7",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.0.0",
    "ch.qos.logback" % "logback-classic" % "1.1.2" % "runtime",
    "ch.qos.logback" % "logback-core" % "1.1.2" % "runtime")

  val deps = List(
    "joda-time" % "joda-time" % "2.9.3",
    "com.thoughtworks.paranamer" % "paranamer" % "2.3",
    "org.apache.commons" % "commons-lang3" % "3.4",
    "org.drools" % "drools-core" % "5.3.0.Final",
    "org.drools" % "knowledge-api" % "5.3.0.Final",
    "org.drools" % "drools-decisiontables" % "5.3.0.Final",
    "org.drools" % "drools-templates" % "5.3.0.Final",
    //"org.drools" % "drools-jsr94" % "5.3.0.Final",
    "org.jbpm" % "jbpm-bpmn2" % "5.1.2.Final",
    "org.jbpm" % "jbpm-flow" % "5.1.2.Final",
    "org.jbpm" % "jbpm-flow-builder" % "5.1.2.Final",
    "com.chuusai" %% "shapeless" % "2.0.0")

  val testingDeps = List(
    "org.specs2" % "specs2-core_2.11" % "3.7.2" % "test")
}
