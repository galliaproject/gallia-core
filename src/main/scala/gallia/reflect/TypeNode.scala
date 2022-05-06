package gallia
package reflect

import aptus.{Anything_, Option_}

import meta._

// ===========================================================================
case class TypeNode(
      leaf : TypeLeaf, // TODO: rename
      args : List[TypeNode]) {

    override def toString: String = formatDefault

    def formatDefault: String = obj.formatPrettyJson
    def obj          : Obj    = TypeNodeObj.typeNode(this)

    // eg for translate type mismatch
    def formatSuccinct: String = TypeNodeUtils.formatSuccinct(this)

    // ---------------------------------------------------------------------------
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

    // ---------------------------------------------------------------------------
    def nodeDesc = NodeDesc.from(this)

    // ===========================================================================
    def isContainedWhatever  : Boolean =
          isWhatever ||
          isWhatevers

        // ---------------------------------------------------------------------------
        // TODO; this == TypeNode.Whatever
        def isWhatever   : Boolean =                          leaf.name == "gallia.whatever.Whatever"
        def isWhatevers  : Boolean = args.headOption.exists(_.leaf.name == "gallia.whatever.Whatever")

        @deprecated def isWhatever0  : Boolean = isWhatever // TODO: t210204170740 - using "0" in places where need to determine if/when must use ContainedWhatever or if isWhatever legit

      // ===========================================================================
      // TODO: rename
      def isContainedWhatever2: Boolean =
          isWhatever2 ||
          isWhatevers2

        // ---------------------------------------------------------------------------
        // TODO; this == TypeNode.TypedWhatever
        def isWhatever2   : Boolean =                          leaf.name == "gallia.whatever.TypedWhatever"
        def isWhatevers2  : Boolean = args.headOption.exists(_.leaf.name == "gallia.whatever.TypedWhatever")

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
    def enmOfnu(c: Cls, path: KPath, multiple: Multiple): Ofnu =
      c .field(path)
        .enmContainee(multiple)
        .pipe(containerType.ofnu)

    // ---------------------------------------------------------------------------
    def containerTypeOpt: Option[Container] = {
           if (isSeq        ) Some(Container._Nes)
      else if (isOptionOfSeq) Some(Container._Pes)
      else if (isOption     ) Some(Container._Opt)
      else                    None }

    // ===========================================================================
    def forceNonBObjOfni                 : Ofni = this.assert(!_.isContainedBObj).pipe(InfoUtils.forceNonBObjOfni)
    def forceNonBObjInfo                 : Info = this.assert(!_.isContainedBObj).pipe(InfoUtils.forceNonBObjInfo)
    def forceNonBObjInfo(enmOpt: _EnmOpt): Info = this.assert(!_.isContainedBObj).pipe(InfoUtils.forceNonBObjInfo(enmOpt))
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
