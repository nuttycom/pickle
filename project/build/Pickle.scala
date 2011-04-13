import sbt._

class Pickle(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
  override def managedStyle = ManagedStyle.Maven
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  val scalasnapshots     = MavenRepository("scala-tools.org Snapshots", "http://scala-tools.org/repo-snapshots/")

  val salt       = "com.nommit"              %% "salt"                    % "1.0"
  val scalaz     = "org.scalaz"              %% "scalaz-core" % "6.0-SNAPSHOT"

  val scalaspec  = "org.scala-tools.testing" %% "specs"       % "1.6.7.2"  % "test"
  val scalacheck = "org.scala-tools.testing" %% "scalacheck"  % "1.8"      % "test"
}

// vim: set ts=4 sw=4 et:
