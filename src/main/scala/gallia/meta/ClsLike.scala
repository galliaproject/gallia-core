package gallia
package meta

import aptus.{Seq_, Option_}

// ===========================================================================
trait ClsLike { // read-only part
      ignored: ClsHelper =>
    protected val _fields: Seq[Fld]

    // ---------------------------------------------------------------------------
    // optimization for runtime validation
    val lookup: Map[Key, Fld]

    val keySet   : Set[Key]
    val keyVector: Vector[Key]

    val requiredKeys  : Seq[Key]
    val requiredKeySet: Set[Key]

    // ===========================================================================
    private def _field (path: RPathW): Fld         = field (path.value)
    private def _field_(path: RPathW): Option[Fld] = field_(path.value)

    // ---------------------------------------------------------------------------
    def size: Int = _fields.size

    // ---------------------------------------------------------------------------
    def  keys: Seq[ Key] = _fields.toList.map(_.key)
    def skeys: Seq[SKey] = _fields.toList.map(_.key.name)
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

    def filter2(pred: FldLike => Boolean): Seq[KPath] = _fields.filter(pred).map(x => KPath.from(x.key))
    def filter3(pred: FldLike => Boolean): Seq[KPath] = _filter   (Nil, pred).map(KPath.opt(_).force)
    def filter5(pred: PNF     => Boolean): Seq[KPath] = _filterPNF(Nil, pred).map(KPath.opt(_).force)

    // ---------------------------------------------------------------------------
    def contains(key  : Key  ): Boolean = keySet.contains(key)
    def contains(path : KPath): Boolean =
      path.tailPair match {
        case (leaf  , None      ) => contains(leaf)
        case (parent, Some(tail)) => _field_(parent).flatMap(_.info.nestingTypeOpt).exists(_.contains(tail)) }

    // ===========================================================================
    // TODO: @delegate to fld-like
    def isNesting (path: KPathW): Boolean = _field(path.value).isNesting
    def isMultiple(path: KPathW): Boolean = _field(path.value).isMultiple
    def isRequired(path: KPathW): Boolean = _field(path.value).isRequired

    // ---------------------------------------------------------------------------
    def isLeaf    (path: KPathW): Boolean = _field(path.value).isNotNesting
    def isScalar  (path: KPathW): Boolean = _field(path.value).isNotMultiple // "scalar" term ok with option?
    def isOptional(path: KPathW): Boolean = !isRequired(path)

    // ===========================================================================
    @PartialTypeMatching
      def isString (path: KPathW): Boolean = _field(path.value).isString
      def isInt    (path: KPathW): Boolean = _field(path.value).isInt
      def isDouble (path: KPathW): Boolean = _field(path.value).isDouble
      def isBoolean(path: KPathW): Boolean = _field(path.value).isBoolean

    // ===========================================================================
    //TODO: get fld-like, nested cls-like
}

// ===========================================================================
