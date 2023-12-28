package gallia
package reflect

// ===========================================================================
/** a simple wrapper for enum values */ case class EnumValue(stringValue: EntryNameString) extends AnyVal {
  override def toString: String = stringValue /* used by convert(myEnum).toStr */

  def enumEntry[E <: enumeratum.EnumEntry](instantiator: Instantiator): E = stringValue.pipe(instantiator.withEnumeratumName).asInstanceOf[E] }

// ===========================================================================
