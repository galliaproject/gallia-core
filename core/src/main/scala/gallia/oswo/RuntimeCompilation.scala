package gallia
package oswo

// ===========================================================================
trait RuntimeCompilation {
  def warmUp(): Unit

  // ---------------------------------------------------------------------------
  /** source shouldn't contain package statements (TODO: why?) */
  def objectInstance[A](objectDefinition: SourceString, objectName: aptus.Name /* TODO: parse source to extract name rather? */): A }

// ===========================================================================
