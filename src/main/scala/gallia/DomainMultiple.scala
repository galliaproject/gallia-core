package gallia

import aptus.{String_, Seq_}
import aptus.MirrorIndex

// ===========================================================================
case class Indicez(values: Seq[MirrorIndex]) extends Iterable[MirrorIndex] {
  def iterator = values.iterator
}

// ===========================================================================
case class Keyz(values: Seq[Key]) extends Seq[Key] {
      def iterator        = values.iterator
      def apply(idx: Int) = values(idx)
      def length: Int     = values.length

    def skeys   : Seq[SKey] = values.map(_.name)
    def valueSet: Set[ Key] = values.toSet

    def duplicates: Keyz = Keyz(values.duplicates)

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String = values match {
          case Nil => "x"
          case seq => seq.join(" ~ ") }

    // ---------------------------------------------------------------------------
    def union  (keys: Keyz): Keyz = Keyz(this.values ++ keys.values)
    def prepend(key : Key ): Keyz = Keyz(key +: values)
    def  append(key : Key ): Keyz = Keyz(       values :+ key)

    // ---------------------------------------------------------------------------
    def renz  : Renz   = values.map(Ren  .from(_)).pipe(Renz  .apply)
    def kpathz: KPathz = values.map(KPath.from(_)).pipe(KPathz.apply)
    def qpathz: RPathz = values.map(RPath.from(_)).pipe(RPathz.apply)
  }

  // ===========================================================================
  object Keyz {
    val Empty = Keyz.apply(Nil)

    // ---------------------------------------------------------------------------
    def from(key: KeyW)                              : Keyz = Keyz(List(key.value))
    def from(key1: KeyW,             more: Seq[KeyW]): Keyz = Keyz((     key1        +: more).map(_.value))
    def from(key1: KeyW, key2: KeyW, more: Seq[KeyW]): Keyz = Keyz((List(key1, key2) ++ more).map(_.value))
  }

// ===========================================================================
case class Renz(values: Seq[Ren]) extends Seq[Ren] {
      def iterator        = values.iterator
      def apply(idx: Int) = values(idx)
      def length: Int     = values.length

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String = values match {
          case Nil => "x"
          case seq => seq.map(_.formatDefault).join(" ~ ") }

    // ---------------------------------------------------------------------------
    def union  (keys: Renz): Renz = Renz(this.values ++ keys.values)
    def prepend(key : Ren ): Renz = Renz(key +: values)
    def  append(key : Ren ): Renz = Renz(       values :+ key)

    // ---------------------------------------------------------------------------
    def froms = values.map(_.from).pipe(Keyz.apply)
    def tos   = values.map(_.to  ).pipe(Keyz.apply)

    @deprecated def fromsFX = values.map(_.from).pipe(Keyz.apply) // FIXME: see t210110104437

    // ---------------------------------------------------------------------------
    def qpathz: RPathz = RPathz(values.map(_.qpath))
  }

  // ===========================================================================
  object Renz {
    def from(key: RenW)                              : Renz = Renz(List(key.value))
    def from(key1: RenW,             more: Seq[RenW]): Renz = Renz((     key1        +: more).map(_.value))
    def from(key1: RenW, key2: RenW, more: Seq[RenW]): Renz = Renz((List(key1, key2) ++ more).map(_.value))
  }

// ===========================================================================
case class KPathz(values: Seq[KPath]) extends Seq[KPath] {
      def iterator        = values.iterator
      def apply(idx: Int) = values(idx)
      def length: Int     = values.length

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String = values match {
        case Nil => "x"
        case seq => seq.map(_.formatDebug).mkString("[", ", ", "]") }

    // ---------------------------------------------------------------------------
    def union  (paths: KPathz): KPathz = KPathz(this.values ++ paths.values)
    def prepend(path : KPath ): KPathz = KPathz(path +: values)
    def  append(path : KPath ): KPathz = KPathz(        values :+ path)

    // ---------------------------------------------------------------------------
    def qpathz: RPathz = values.map(_.qpath).pipe(RPathz.apply)

                def forceKeyz  : Keyz = values.map(_.forceLeaf).pipe(Keyz.apply)
    @deprecated def forceKeyzFX: Keyz = values.map(_.forceLeaf).pipe(Keyz.apply) // FIXME: see t210110104437

    def isDisjointWith(that: KPathz): Boolean = this.values.intersect(that.values).isEmpty

    // ---------------------------------------------------------------------------
    // must be prevalidated
    def mapping: Map[Key, Option[KPathz]] = // TODO: rename
      values
        .map(_.tailPair)
        .groupByKey
        .mapValues {
          case Seq(None) => None
          case multiple  => Some(KPathz(multiple.flatten)) }
        .toMap
  }

  // ===========================================================================
  object KPathz {
    def from(key: KeyW): KPathz = KPathz(List(KPath.from(key)))

    // ===========================================================================
    object Implicits {
      implicit def to(x: Seq[KPath]): KPathz = KPathz(x)
    }
  }

// ===========================================================================
case class RPathz(values: Seq[RPath]) extends Seq[RPath] {
      def iterator        = values.iterator
      def apply(idx: Int) = values(idx)
      def length: Int     = values.length

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String = values match {
        case Nil => "x"
        case seq => seq.map(_.formatDebug).mkString("[", ", ", "]") }

    // ---------------------------------------------------------------------------
    def union  (paths: RPathz): RPathz = RPathz(this.values ++ paths.values)
    def prepend(path : RPath ): RPathz = RPathz(path +: values)
    def  append(path : RPath ): RPathz = RPathz(        values :+ path)

    // ---------------------------------------------------------------------------
    def froms: Seq[KPath] = values.map(_.from)
    def fromz:     KPathz = values.map(_.from).pipe(KPathz.apply)

    def   tos: Seq[KPath] = values.flatMap(_.toOpt)
    def   toz:     KPathz = values.flatMap(_.toOpt).pipe(KPathz.apply)

    // ---------------------------------------------------------------------------
    //FIXME: see t210110104437
    @deprecated def force1FX: RPath      = values.force.one
    @deprecated def from1FX : KPath      = froms.force.one
    @deprecated def from1KFX: Key        = froms.force.one.key
    @deprecated def fromsFX : Seq[KPath] = froms
    @deprecated def fromzFX : KPathz     = fromz

    // ---------------------------------------------------------------------------
    def forceKPathz: KPathz = values.map(_.forceKPath).pipe(KPathz.apply)
    def forceRenz  : Renz   = values.map(_.forceLeaf).pipe(Renz.apply)
    @deprecated
    def forceRenzFX: Renz   = values.map(_.forceLeaf).pipe(Renz.apply) // FIXME (t210110104437)
  }

  // ===========================================================================
  object RPathz {
    def from(path: KPathW): RPathz = RPathz(List(RPath.from(path)))
  }

// ===========================================================================
case class SPathz(parent: Option[KPath], keyz: Keyz) { // TODO: keep?
    // eg 'p |> ('f, 'g)
    override def toString: String = formatDefault
      def formatDefault: String =
        parent.map(_.formatDebug.append(" |> ")).getOrElse("") +
        keyz.formatDefault.surroundWith("(", ")")
}

// ===========================================================================
