// ===========================================================================
object GalliaScalacOptions {

  val commonOptions = Seq(
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-deprecation",

      "-language:implicitConversions")

  // ===========================================================================
  val commonOptionsExcept212 = Seq(      
      "-Wunused:implicits",
      "-Wunused:explicits",
      "-Wunused:imports",
      "-Wunused:locals",
      "-Wunused:params",
      "-Wunused:privates")

  // ===========================================================================
  val scala3Options   = commonOptions ++ commonOptionsExcept212 ++ Seq("-no-indent",       "-Wvalue-discard")
  val scala213Options = commonOptions ++ commonOptionsExcept212 ++ Seq("-Wdead-code", "-Ywarn-value-discard")
  val scala212Options = commonOptions ++                           Seq(               "-Ywarn-value-discard", "-Ywarn-unused-import" ) }

// ===========================================================================

