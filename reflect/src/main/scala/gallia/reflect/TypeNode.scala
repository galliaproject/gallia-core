package gallia
package reflect

import aptus.{Seq_, String_}
import aptus.Option_

// ===========================================================================
case class TypeNode(
    leaf : TypeLeaf, // TODO: rename
    args : List[TypeNode]) {

  // ---------------------------------------------------------------------------
  override def toString = formatDebug

    def formatDebug: String = // TODO: use PPrint
      leaf.formatDebug.newline +
      (if (args.isEmpty) ""
       else              args.map(_.formatDebug).section)

  // ===========================================================================
  // TODO: t231017103243 - use lens lib

  def dataClass      (value: Boolean): TypeNode = copy(leaf = leaf.dataClass      (value))
  def galliaEnumValue(value: Boolean): TypeNode = copy(leaf = leaf.galliaEnumValue(value))
  def bytes          (value: Boolean): TypeNode = copy(leaf = leaf.bytes          (value))
  def inheritsSeq    (value: Boolean): TypeNode = copy(leaf = leaf.inheritsSeq    (value))

  def fields(field1: Field, more: Field*) = copy(leaf = leaf.copy(fields = field1 +: more))

  def typeArg(value: TypeNode) = copy(args = List(value))

  // ===========================================================================
  def complex: Boolean = leaf.dataClass || leaf.fields.nonEmpty || args.nonEmpty

  // ---------------------------------------------------------------------------
  /** including "one" as container */
  def isContainedDataClass      : Boolean = validContainerOpt.exists(_.dataClass)
  def isContainedGalliaEnumValue: Boolean = validContainerOpt.exists(_.galliaEnumValue)
  def isContainedEnumeratum     : Boolean = validContainerOpt.exists(_.isEnumeratum)

  // ---------------------------------------------------------------------------
  def flattenedEnumValueNames: Seq[String] = leaf.enumeratumValueNamesOpt.getOrElse(Nil)

  // ---------------------------------------------------------------------------
  def fieldNames: Seq[String] = leaf.fields.map(_.key) // may be empty if not a data class

  // ===========================================================================
  def ifApplicable(f: Any => Any): Any /* value */ => Any /* value */ =
      value =>
        if (sameType(value)) f(value)
        else                   value

    // ---------------------------------------------------------------------------
    // TODO: t220411094433 - hopefully there's a cleaner way...
    private def sameType(value: Any): Boolean =
      leaf.name == reflect.FullName.fromRuntimeValue(value)

  // ===========================================================================
  def forceSoleTypeArg: TypeNode = args.force.one

  // ===========================================================================
  def forceValidContainer: TypeLeaf = validContainerOpt.force

    /** should return some if valid, just removing any Option/Seq containers */
    def validContainerOpt: Option[TypeLeaf] = TypeNodeUtils.validContainerOpt(this)

  // ---------------------------------------------------------------------------
  /** e.g for HeadV values, not necessarily restricted by the One/Opt/Nes/Pes paradigm (eg Seq[Option] is valid) */
  private[gallia] def underlyingFullName = removeAllContainers.leaf.fullName

    // ---------------------------------------------------------------------------
    private def removeAllContainers: TypeNode =
           if (isSeq)    forceSoleTypeArg.removeAllContainers
      else if (isOption) forceSoleTypeArg.removeAllContainers
      else               this

  // ===========================================================================
  def isContainedWhatever: Boolean =
        isWhatever ||
        isWhatevers

      // ---------------------------------------------------------------------------
      // TODO; this == TypeNode.Whatever
      private def isWhatever   : Boolean =                          leaf.fullName.isGalliaWhatever
      private def isWhatevers  : Boolean = args.headOption.exists(_.leaf.fullName.isGalliaWhatever)

      // ---------------------------------------------------------------------------
      // TODO: t210204170740 - using "0" in places where need to determine if/when must use ContainedWhatever or if isWhatever legit
      @deprecated def isWhatever0: Boolean = isWhatever

  // ---------------------------------------------------------------------------
  def isContainedBObj: Boolean = isBObj || isBObjs
    def isBObj     : Boolean = leaf.fullName.isGalliaBObj
    def isBObjs    : Boolean = isSeq && args.headOption.exists(_.isBObj)

  // ===========================================================================
  def isOptionOfSeq : Boolean = isOption && args.headOption.exists(_.isSeq)
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

  // ===========================================================================
  def wrapInOption: TypeNode = TypeNodeBuiltIns.scalaOption(this)
  def wrapInSeq   : TypeNode = TypeNodeBuiltIns.scalaSeq   (this)

  // ===========================================================================
  // meant for post-validation
  def containerType: Container = containerTypeOpt.getOrElse(Container._One)

  // ---------------------------------------------------------------------------
  def containerTypeOpt: Option[Container] = {
         if (isSeq        ) Some(Container._Nes)
    else if (isOptionOfSeq) Some(Container._Pes)
    else if (isOption     ) Some(Container._Opt)
    else                    None } }

// ===========================================================================
object TypeNode {
  val Dummy: TypeNode = TypeNode(TypeLeaf.Dummy, List.empty)

  // ---------------------------------------------------------------------------
  def trivial(name: FullNameString): TypeNode =
    TypeNode(
      leaf = TypeLeaf.trivial(name),
      args = List.empty) }

// ===========================================================================
