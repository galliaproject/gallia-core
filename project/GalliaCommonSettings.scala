import sbt     ._
import sbt.Keys._

// ===========================================================================
object GalliaCommonSettings {

  val mainSettings = Seq(
         scalaVersion  := GalliaScalaVersions.supported.head,
    crossScalaVersions := GalliaScalaVersions.supported,
    // ---------------------------------------------------------------------------
    scalacOptions     ++= (scalaBinaryVersion.value match {
      case "3"    => GalliaScalacOptions.scala3Options
      case "2.13" => GalliaScalacOptions.scala213Options
      case "2.12" => GalliaScalacOptions.scala212Options })) }

// ===========================================================================

