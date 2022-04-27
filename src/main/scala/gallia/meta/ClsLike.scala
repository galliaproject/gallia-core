package gallia
package meta

import aptus.{Seq_, Option_}

// ===========================================================================
trait ClsLike { // read-only part
      ignored: ClsHelper =>
    protected val fields: Seq[Fld]

    // ---------------------------------------------------------------------------
    // optimization for runtime validation
    val lookup: Map[Key, Fld]

    val keySet   : Set   [Key]
    val keyVector: Vector[Key]

    val requiredKeys  : Seq[Key]
    val requiredKeySet: Set[Key]

    def indexOf(key: Key): Int = keys.indexOf(key)

    // ===========================================================================
    def field (path: RPathW):        Fld  = field (path.value)
    def field_(path: RPathW): Option[Fld] = field_(path.value)

    // ---------------------------------------------------------------------------
    def soleField: Fld = fields.force.one

    // ---------------------------------------------------------------------------
    def forceNestedClass(key: Key  ): Cls = field(key).forceNestedClass
    def forceNestedClass(key: KPath): Cls = field(key).forceNestedClass

    def forceBasicType(key: Key  ): BasicType = field(key).forceBasicType
    def forceBasicType(key: KPath): BasicType = field(key).forceBasicType

    // ===========================================================================
    def size: Int = fields.size

    // ---------------------------------------------------------------------------
    def  keys: Seq[ Key] = fields.toList.map(_.key)
    def skeys: Seq[SKey] = fields.toList.map(_.key.name)
    def  keyz:     Keyz  = Keyz(keys)

    // ---------------------------------------------------------------------------
    def leafPaths: Seq[KPath] = _leafPaths(parent = Nil).map(KPath.opt(_).force)
    def  allPaths: Seq[KPath] =  _allPaths(parent = Nil).map(KPath.opt(_).force)

    // ---------------------------------------------------------------------------
    def complementKeyz(x: Key ): Keyz = keys.filterNot(_ == x).pipe(Keyz.apply) //TODO: ensure match...
    def complementKeyz(x: Keyz): Keyz = keys.diff(x.values)   .pipe(Keyz.apply) //TODO: ensure match...
    // TODO: complement path?

    // ---------------------------------------------------------------------------
    def filterKey  (pred:  Key => Boolean): Seq[Key] = keys.filter  (pred)
    def filterSKeys(pred: SKey => Boolean): Seq[Key] = keys.filterBy(pred)(_.name)

    def filter2(pred: FldLike => Boolean): Seq[KPath] = fields.filter(pred).map(x => KPath.from(x.key))
    def filter3(pred: FldLike => Boolean): Seq[KPath] = _filter   (Nil, pred).map(KPath.opt(_).force)
    def filter5(pred: PNF     => Boolean): Seq[KPath] = _filterPNF(Nil, pred).map(KPath.opt(_).force)

    // ---------------------------------------------------------------------------
    def contains(key  : Key  ): Boolean = keySet.contains(key)
    def contains(path : KPath): Boolean =
      path.tailPair match {
        case (leaf  , None      ) => contains(leaf)
        case (parent, Some(tail)) => field_(parent).flatMap(_.nestedClassOpt).exists(_.contains(tail)) }

    // ===========================================================================
    def hasNesting : Boolean = fields.exists(_.hasNesting)
    def hasMultiple: Boolean = fields.exists(_.hasMultiple)
    def hasUnions  : Boolean = fields.exists(_.isUnionType)

    def areAllNonRequired(keyz: Keyz): Boolean = keyz.map(field(_)).forall(!_.isRequired)

    // ---------------------------------------------------------------------------
    def hasNesting (path: KPathW): Boolean = field(path.value).ofni.hasNesting
    def hasMultiple(path: KPathW): Boolean = field(path.value).ofni.hasMultiple

    // ---------------------------------------------------------------------------
    def isMultiple(path: KPathW): Boolean = field(path.value).ofni.areAllMultiple
    def isSingle  (path: KPathW): Boolean = field(path.value).ofni.areAllSingle

    // ---------------------------------------------------------------------------
    def isRequired(path: KPathW): Boolean = field(path.value).isRequired
    def isOptional(path: KPathW): Boolean = field(path.value).isOptional

    // ===========================================================================
    //TODO: get fld-like, nested cls-like
}

// ===========================================================================
