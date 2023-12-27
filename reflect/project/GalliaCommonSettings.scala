import sbt     ._
import sbt.Keys._

// ===========================================================================
object GalliaCommonSettings {
  val CurrentGalliaVersion = "0.6.0-SNAPSHOT"

  // ---------------------------------------------------------------------------
  val mainSettings = Seq(
    organizationHomepage := Some(url("https://github.com/galliaproject")),
    startYear            := Some(2021),
    developers           :=
      List(Developer(
        id    = "anthony-cros",
        name  = "Anthony Cros",
        email = "contact.galliaproject@gmail.com",
        url   = url("https://github.com/anthony-cros") )),

    // ---------------------------------------------------------------------------
         scalaVersion  := GalliaScalaVersions.supported.head,
    crossScalaVersions := GalliaScalaVersions.supported,
    // ---------------------------------------------------------------------------
    scalacOptions     ++= (scalaBinaryVersion.value match {
      case "3"    => GalliaScalacOptions.scala3Options
      case "2.13" => GalliaScalacOptions.scala213Options
      case "2.12" => GalliaScalacOptions.scala212Options })) }

// ===========================================================================

