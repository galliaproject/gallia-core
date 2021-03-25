package gallia.reflect

import aptus.Anything_
import aptus.Option_

import gallia._
import gallia.meta._

// ===========================================================================
case class TypeNode(
      leaf : TypeLeaf,
      args : List[TypeNode]) {

    override def toString: String = formatDefault

    def formatDefault: String  = obj.formatPrettyJson
    def obj          : gallia.Obj = TypeNodeObj.typeNode(this)

    // eg for translate type mismatch
    def formatSuccinct: String = TypeNodeUtils.formatSuccinct(this)

    // ---------------------------------------------------------------------------
    def notEquilalent(that: TypeNode): Boolean = this.unaliased != that.unaliased
    def    equilalent(that: TypeNode): Boolean = this.unaliased == that.unaliased

    def unaliased: TypeNode = TypeNode(leaf.unaliased, args.map(_.unaliased))

    def complex: Boolean = leaf.dataClass || leaf.fields.nonEmpty || args.nonEmpty

    // ---------------------------------------------------------------------------
    /** including "one" as container */
    def isContainedDataClass: Boolean = validContainerOpt.exists(_.dataClass)

    /** should return some if valid, just removing any Option/Seq containers */
    def validContainerOpt: Option[TypeLeaf] = TypeNodeUtils.validContainerOpt(this)

    def forceValidContainer: TypeLeaf = validContainerOpt.force

    // ---------------------------------------------------------------------------
    def nodeDesc = NodeDesc.from(this)

    // ===========================================================================
    def isContainedWhatever  : Boolean =
          isWhatever ||
          isWhatevers

        // ---------------------------------------------------------------------------
        // TODO; this == TypeNode.Whatever
        def isWhatever   : Boolean =                          leaf.name == "gallia.Whatever"
        def isWhatevers  : Boolean = args.headOption.exists(_.leaf.name == "gallia.Whatever")

        @deprecated def isWhatever0  : Boolean = isWhatever // TODO: t210204170740 - using "0" in places where need to determine if/when must use ContainedWhatever or if isWhatever legit

      // ===========================================================================
      // TODO: rename
      def isContainedWhatever2: Boolean =
          isWhatever2 ||
          isWhatevers2

        // ---------------------------------------------------------------------------
        // TODO; this == TypeNode.TypedWhatever
        def isWhatever2   : Boolean =                          leaf.name == "gallia.TypedWhatever"
        def isWhatevers2  : Boolean = args.headOption.exists(_.leaf.name == "gallia.TypedWhatever")

    // ===========================================================================
    def isContainedBObj: Boolean = isBObj || isBObjs
      def isBObj     : Boolean = leaf.name == _BObj
      def isBObjs    : Boolean = isSeq && args.head.isBObj

    // ---------------------------------------------------------------------------
    //TODO: more stringent
    def isHeadU    : Boolean = leaf.name == _HeadU
    def isHeadZ    : Boolean = leaf.name == _HeadZ

    // ---------------------------------------------------------------------------
    def isOptionOfSeq : Boolean = isOption && args.head.isSeq // isSome && args.head.isSeq
    def isSeq         : Boolean = leaf.isSeq    && args.size == 1
    def isOption      : Boolean = leaf.isOption && args.size == 1

    // ---------------------------------------------------------------------------
    def isMultiple   : Boolean = isSeq || isOptionOfSeq
    def isNotRequired: Boolean = leaf.isOption

    def isNotOne  : Boolean = isSeq || isOptionOfSeq || isOption

    // ---------------------------------------------------------------------------
    def isSome           : Boolean = leaf.isSome && args.size == 1
    def isNone           : Boolean = leaf.isNone
    def isUnqualifiedNone: Boolean = leaf.isNone && args.isEmpty

    // ---------------------------------------------------------------------------
    def normalizeSome: TypeNode = if (isSome) forceSomeToOption else this

      private def forceSomeToOption: TypeNode = copy(leaf = TypeNode.parse[Option[_]].leaf)

    // ===========================================================================
    // meant for post-validation
    def containerType: Container = containerTypeOpt.getOrElse(Container._One)

    // ---------------------------------------------------------------------------
    def containerTypeOpt: Option[Container] =
           if (isSeq        ) Some(Container._Nes)
      else if (isOptionOfSeq) Some(Container._Pes)
      else if (isOption     ) Some(Container._Opt)
      else                    None

    // ===========================================================================
    def forceNonBObjInfo: Info = this.assert(!_.isContainedBObj).thn(InfoUtils.forceNonBObjInfo)   
  }

  // ===========================================================================
  object TypeNode {

    def parse[A: WTT]: TypeNode = parse(scala.reflect.runtime.universe.weakTypeTag[A].tpe)

    def parse(tpe: UType): TypeNode =
      TypeNode(
          leaf = TypeLeafParser(tpe),
          args = tpe.typeArgs.map(parse))

  }

// ===========================================================================
