// ===========================================================================
package gallia {
  object ScalaVersion extends ScalaVersionTrait {
    /* scala.util.Properties.versionNumberString doesn't seem to work well when running test from sbt */
    final override protected val _isScala2 = true } }

// ===========================================================================