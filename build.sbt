organization := "org.inp"

name := "scalatra-prnd"

version := "0.1.0"

scalaVersion := "2.9.1"

seq(webSettings :_*)

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.0.2",
  "org.scalatra" %% "scalatra-scalate" % "2.0.2",
  "org.scalatra" %% "scalatra-specs2" % "2.0.2" % "test",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2",
  "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "7.4.5.v20110725" % "container",
  "javax.servlet" % "servlet-api" % "2.5" % "provided",
  "org.squeryl" %% "squeryl" % "0.9.5",
  "mysql" % "mysql-connector-java" % "5.1.10"
)

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

excludeFilter in unmanagedSources := "WebApp.scala" 

port in container.Configuration := 7000