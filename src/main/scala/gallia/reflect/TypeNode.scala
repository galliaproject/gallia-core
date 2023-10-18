package gallia
package reflect

import aptus.Option_

// ===========================================================================
case class TypeNode(
    leaf : TypeLeaf, // TODO: rename
    args : List[TypeNode]) {

  // ===========================================================================
  // TODO: t231017103243 - use lens lib
  def alias      (value   :        Alias ): TypeNode = copy(leaf = leaf.alias(value))
  def alias      (valueOpt: Option[Alias]): TypeNode = copy(leaf = leaf.alias(valueOpt))
  def inScopeName(value   : InScopeName  ): TypeNode = copy(leaf = leaf.inScopeName(value))

  // ===========================================================================
  def notEquilalent(that: TypeNode): Boolean = this.unaliased != that.unaliased
  def    equilalent(that: TypeNode): Boolean = this.unaliased == that.unaliased

  def unaliased: TypeNode = TypeNode(leaf.unaliased, args.map(_.unaliased))

  def complex: Boolean = leaf.dataClass || leaf.fields.nonEmpty || args.nonEmpty

  // ---------------------------------------------------------------------------
  /** including "one" as container */
  def isContainedDataClass : Boolean = validContainerOpt.exists(_.dataClass)
  def isContainedEnum      : Boolean = validContainerOpt.exists(_.enm)
  def isContainedEnumeratum: Boolean = validContainerOpt.exists(_.isEnumeratum)

  // ---------------------------------------------------------------------------
  def forceValidContainer: TypeLeaf = validContainerOpt.force

    /** should return some if valid, just removing any Option/Seq containers */
    def validContainerOpt: Option[TypeLeaf] = TypeNodeUtils.validContainerOpt(this)

  // ===========================================================================
  def isContainedWhatever  : Boolean =
        isWhatever ||
        isWhatevers

      // ---------------------------------------------------------------------------
      // TODO; this == TypeNode.Whatever
      def isWhatever   : Boolean =                          leaf.fullName.isGalliaWhatever
      def isWhatevers  : Boolean = args.headOption.exists(_.leaf.fullName.isGalliaWhatever)

      @deprecated def isWhatever0  : Boolean = isWhatever // TODO: t210204170740 - using "0" in places where need to determine if/when must use ContainedWhatever or if isWhatever legit

    // ===========================================================================
    // TODO: rename
    def isContainedWhatever2: Boolean =
        isWhatever2 ||
        isWhatevers2

      // ---------------------------------------------------------------------------
      // TODO; this == TypeNode.TypedWhatever
      def isWhatever2   : Boolean =                          leaf.fullName.isGalliaTypedWhatever
      def isWhatevers2  : Boolean = args.headOption.exists(_.leaf.fullName.isGalliaTypedWhatever)

  // ===========================================================================
  def isContainedBObj: Boolean = isBObj || isBObjs
    def isBObj     : Boolean = leaf.fullName.isGalliaBObj
    def isBObjs    : Boolean = isSeq && args.head.isBObj

  // ---------------------------------------------------------------------------
  //TODO: more stringent
  def isHeadU    : Boolean = leaf.fullName.isGalliaHeadU
  def isHeadZ    : Boolean = leaf.fullName.isGalliaHeadZ

  // ---------------------------------------------------------------------------
  def isOptionOfSeq : Boolean = isOption && args.head.isSeq // isSome && args.head.isSeq
  def isSeq         : Boolean = leaf.isSeq    && args.size == 1
  def isOption      : Boolean = leaf.isOption && args.size == 1

  // ---------------------------------------------------------------------------
  def isMultiple: Boolean = isSeq || isOptionOfSeq
  def isOptional: Boolean = leaf.isOption

  def isNotOne  : Boolean = isSeq || isOptionOfSeq || isOption

  // ---------------------------------------------------------------------------
  def isSome           : Boolean = leaf.isSome && args.size == 1
  def isNone           : Boolean = leaf.isNone
  def isUnqualifiedNone: Boolean = leaf.isNone && args.isEmpty

  // ---------------------------------------------------------------------------
  def normalizeSome: TypeNode = if (isSome) forceSomeToOption else this

    private def forceSomeToOption: TypeNode = copy(leaf = typeNode[Option[_]].leaf)

  // ===========================================================================
  // meant for post-validation
  def containerType: Container = containerTypeOpt.getOrElse(Container._One)

  // ---------------------------------------------------------------------------
  def containerTypeOpt: Option[Container] = {
         if (isSeq        ) Some(Container._Nes)
    else if (isOptionOfSeq) Some(Container._Pes)
    else if (isOption     ) Some(Container._Opt)
    else                    None }
}

// ===========================================================================
object TypeNode {
  val Dummy               : TypeNode = TypeNode(TypeLeaf.Dummy       , Nil)
  def debug(value: String): TypeNode = TypeNode(TypeLeaf.debug(value), Nil)

  // ---------------------------------------------------------------------------
  def trivial(name: String, alias: String): TypeNode = trivial(name).alias(alias)
  def trivial(name: String)               : TypeNode =
    TypeNode(
      leaf = TypeLeaf.trivial(name),
      args = Nil)

  // ===========================================================================
  private val _ScalaInt    = "scala.Int"
  private val _ScalaString = "scala.Predef.String" // just an alias, unlike eg scala.Int

  // ---------------------------------------------------------------------------
  private val _JavaInt     = "java.lang.Integer"
  private val _JavaString  = "java.lang.String"

  // ===========================================================================
  val JavaString  = TypeNode.trivial(_JavaString, alias = "String") // same for scala.Predef.String
  val ScalaString = JavaString // just an alias, unlias eg scala.Int
  val String      = JavaString // for convenience

  // ---------------------------------------------------------------------------
  val JavaInt    = TypeNode.trivial(_JavaInt   , alias = "Integer")
  val ScalaInt   = TypeNode.trivial(_ScalaInt  , alias = "Int") }

// ===========================================================================
