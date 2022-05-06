package gallia
package meta

import aptus._

// ===========================================================================
trait OfniLike extends InfosLike {
  protected val _ofni: Ofni

  // ===========================================================================
  def isType[T: WTT]: Boolean = isType(TypeNode.parse[T])

    def isType(node: TypeNode): Boolean =
      node
        .assert(!_.isContainedBObj) // has already been validated by here (see 201014103336)
        .forceNonBObjOfni
        .pipe(_ == this)

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  def isOneString  : Boolean = isRequired && areAllSingle && isString
  def isOneInt     : Boolean = isRequired && areAllSingle && isInt
  def isOneDouble  : Boolean = isRequired && areAllSingle && isDouble
  def isOneBoolean : Boolean = isRequired && areAllSingle && isBoolean

  // ---------------------------------------------------------------------------
  def isRequired: Boolean = !_ofni.optional
  def isOptional: Boolean =  _ofni.optional

  // ---------------------------------------------------------------------------
  def isOne: Boolean = isRequired && areAllSingle
  def isOpt: Boolean = isOptional && areAllSingle
  def isNes: Boolean = isRequired && areAllMultiple
  def isPes: Boolean = isOptional && areAllMultiple

  // ===========================================================================
  def trivialContaineeOpt: Option[Containee] =  _ofni.infos.ifOneOpt.flatMap(_.inSomeIf(_ => isRequired)).map(_.containee)

  // ===========================================================================
  private[gallia] def enmOfnu     (multiple: Boolean): Ofnu      = enmInfo(multiple).ofnu(_ofni.optional)
  private[gallia] def enmContainee(multiple: Boolean): Containee = enmInfo(multiple).containee
  private[gallia] def enmInfo     (multiple: Boolean): Info      =  _ofni.infos.filter(_.isEnmMatching(multiple)).force.one
}

// ===========================================================================
