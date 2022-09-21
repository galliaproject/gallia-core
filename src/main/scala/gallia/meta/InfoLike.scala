package gallia
package meta

import aptus._

// ===========================================================================
trait InfoLike extends SubInfosLike {
  protected val _info: Info
  import _info._

  // ===========================================================================
  @deprecated def container1: Container = Container.from(optional, subInfo1.multiple)

  // ---------------------------------------------------------------------------
  def forceContainer: Container = Container.from(optional, union.map(_.multiple).distinct.force.one)
  def forceInfo1     : Info1      = forceSubInfo.pipe { subInfo => Info1(optional, subInfo.multiple, subInfo.valueType) }

  // ---------------------------------------------------------------------------
  def toFld(key: KeyW): Fld = Fld(key.value, _info)

  // ===========================================================================
  def isType[T: WTT]: Boolean = isType(TypeNode.parse[T])

    def isType(node: TypeNode): Boolean =
      node
        .assert(!_.isContainedBObj) // has already been validated by here (see 201014103336)
        .forceNonBObjInfo
        .pipe(_ == this)

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  def isOneString  : Boolean = isRequired && areAllSingle && isString
  def isOneInt     : Boolean = isRequired && areAllSingle && isInt
  def isOneDouble  : Boolean = isRequired && areAllSingle && isDouble
  def isOneBoolean : Boolean = isRequired && areAllSingle && isBoolean

  // ---------------------------------------------------------------------------
  def isRequired: Boolean = !_info.optional
  def isOptional: Boolean =  _info.optional

  // ---------------------------------------------------------------------------
  def isOne: Boolean = isRequired && areAllSingle
  def isOpt: Boolean = isOptional && areAllSingle
  def isNes: Boolean = isRequired && areAllMultiple
  def isPes: Boolean = isOptional && areAllMultiple

  // ===========================================================================
  def trivialValueTypeOpt: Option[ValueType] =  _info.union.ifOneElementOpt.flatMap(_.inSomeIf(_ => isRequired)).map(_.valueType)

  // ===========================================================================
  private[gallia] def enmInfo1    (multiple: Boolean): Info1     = enmSubInfo(multiple).info1(_info.optional)
  private[gallia] def enmValueType(multiple: Boolean): ValueType = enmSubInfo(multiple).valueType
  private[gallia] def enmSubInfo  (multiple: Boolean): SubInfo   = _info.union.filter(_.isEnmMatching(multiple)).force.one

  // ===========================================================================
  /** used trying to fit the data to the schema (as opposed to when we know the data is compliant already) - more costly */
  private[gallia] def valueExtractionWithFailures
          (nesting: Cls       => Multiple => AnyValue)
          (basic  : BasicType => Multiple => AnyValue)
        : AnyValue =
      union match {
        case Seq(sole) =>                            sole.valueExtraction(nesting)(basic)
        case _         => union.flatMap { x => util.Try(x.valueExtraction(nesting)(basic) /* relies on 220615165554 failures */).toOption }.force.one }

  // ---------------------------------------------------------------------------
  /** used when we know the data is compliant with the schema already (as opposed to when we are trying to fit the data to the schema) */
  private[gallia] def valueExtractionWithMatching(debug: String)(value: AnyValue)
          (nesting: Cls       => Multiple => AnyValue)
          (basic  : BasicType => Multiple => AnyValue)
        : AnyValue =
      union match {
        case Seq(sole) => sole.valueExtraction(nesting)(basic)
        case _         => // union type
          (value match {
              case seq: Seq[_] => _Multiple -> seq.head // first suffices since all values will share the same type (+ can't be empty)
              case sgl         => _Single   -> sgl })
            .pipe { case (multiple, x) =>
              BasicType.matchingSubinfos(this)(multiple)(x) }
             match {
              case Seq(sole) => sole.valueExtraction(nesting)(basic)
              case other     => aptus.illegalState(Seq(debug /* eg key */, value, other.size))  } }
}

// ===========================================================================
