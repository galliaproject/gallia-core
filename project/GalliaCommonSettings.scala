import sbt     ._
import sbt.Keys._

// ===========================================================================
object GalliaCommonSettings {
  val scala3   = "3.3.1"
  val scala213 = "2.13.12"
  val scala212 = "2.12.18"
  
  // ---------------------------------------------------------------------------
  val mainSettings = Seq(
         scalaVersion  :=      scala3,
    crossScalaVersions := List(scala3, scala213, scala212),
    // ---------------------------------------------------------------------------
    scalacOptions ++= (scalaBinaryVersion.value match {
      case "3"    => GalliaScalacOptions.scala3Options
      case "2.13" => GalliaScalacOptions.scala213Options
      case "2.12" => GalliaScalacOptions.scala212Options })) }

// ===========================================================================

