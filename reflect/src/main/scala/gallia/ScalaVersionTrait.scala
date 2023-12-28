package gallia

// ===========================================================================
trait ScalaVersionTrait {
  /* scala.util.Properties.versionNumberString doesn't seem to work well when running test from sbt */
  protected def _isScala2: Boolean

  // ---------------------------------------------------------------------------
  def isScala2: Boolean = _isScala2
  final def companionName(name: String): String = if (_isScala2) name else name + "$" }

// ===========================================================================

