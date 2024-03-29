package gallia

import aptus.{Anything_, Seq_}
import aptus.Option_

// ===========================================================================
case class ActualRen(from: Key, to: Key) { 
  require(from != to, from)
  def pair = from -> to }

// ===========================================================================
case class Ren(from: Key, to: Key /* may be the same as from */) { // TODO: t210115153452 - change so as to use Option for to
    override def toString: String = formatDefault
      def formatDefault: String = if (isActual) s"${from.name} ~> ${to.name}" else from.name

    def isActual: Boolean     = from != to

    def toOpt        : Option[Key]        = to.in.someIf(_ != from)
    def actualOptPair: Option[(Key, Key)] = if (isActual) Some(from -> to) else None
    def actualOpt    : Option[ActualRen]  = either.toOption

    def either: Either[Key, ActualRen] = if (from == to) Left(from) else Right(ActualRen(from, to))

    def rpath: RPath = RPath(Nil, this)
    @deprecated def fromFX = from // FIXME: see t210110104437

    def renz = Renz(Seq(this)) }

  // ===========================================================================
  object Ren {
    def from(key: KeyW          ): Ren = Ren(key.value, key.value)
    def from(key: KeyW, to: KeyW): Ren = Ren(key.value, to .value) }

// ===========================================================================
case class KPath(parent: Seq[Key], key: Key) {
    private val all = parent :+ key

    def formatDebug = all.map(_.name).join(" |> ")
    def formatDefault = formatDebug

    override def toString = formatDebug

    def skey = key.name

    def isLeaf     : Boolean = parent.isEmpty
    def forceLeaf  : Key     = this.assert(_.isLeaf   ).key

    @deprecated
    def forceLeafFX:        Key  = this.assert(_.isLeaf   ).key // FIXME
    def leafOpt    : Option[Key] = if (parent.isEmpty) Some(key) else None

    def appendLevel(that: Key): KPath = KPath(all, that)

    // ---------------------------------------------------------------------------
    def first : Key = all.head
    def last  : Key = all.last

    def tail: Option[KPath] = parent.in.noneIf(_.isEmpty).map { x => KPath(x.tail, key) }
    def init: Option[KPath] = parent.in.noneIf(_.isEmpty).map { x => KPath(x.init, x.last) }


    // ---------------------------------------------------------------------------
    def rpath          : RPath = RPath(parent, Ren.from(key))
    def rpath(to: KeyW): RPath = RPath(parent, Ren(key, to.value))

    def rpath(to: Ren  ): RPath = RPath(parent :+ key, to)

    // ---------------------------------------------------------------------------
    // symbolic access

    def |> (that: KeyW): KPath = appendLevel(that.value)
    def |> (that: Ren  ): RPath = RPath(parent :+ key, that)

    def ||> (that: KeyPair): SPathz = ???

    // ---------------------------------------------------------------------------
    def tailPair : (Key, Option[KPath]) = (first, tail)
      /*
        pair match {
          case (leaf  , None      ) => ???
          case (parent, Some(tail)) => ??? }
      */

    // ---------------------------------------------------------------------------
    def initPair : (Option[KPath], Key) = (init, last)
      /*
        pair match {
          case (None      , leaf) => ???
          case (Some(tail), leaf) => ???
      */ }

  // ===========================================================================
  object KPath {
    def from(key: KeyW): KPath = KPath(Nil, key.value)

    def opt(values: Seq[Key]): Option[KPath] = values.in.noneIf(_.isEmpty).map(x => KPath(x.init, x.last)) }

// ===========================================================================
case class RPath(parent: Seq[Key], ren: Ren) {

    override def toString: String = formatDefault
      def formatDefault: String = formatDebug

      def formatDebug = from.formatDebug + (if (ren.isActual) s" ~> ${ren.to.name}" else "")

    // ---------------------------------------------------------------------------
    def prepend(root: KeyW): RPath = RPath(root.value +: parent, ren)
    
    // ---------------------------------------------------------------------------
    def isActual: Boolean = ren.isActual
    def isLeaf  : Boolean = parent.isEmpty

    def forceKPath: KPath = this.ensuring(!_.isActual).from
    def forceLeaf : Ren   = this.ensuring(_.isLeaf   ).ren

    def from : KPath  = KPath(parent, ren.from)

    //FIXME: see t210110104437
    @deprecated def fromFX: KPath = KPath(parent, ren.from)
    @deprecated def renFX : Ren   = ren
    @deprecated def forceKPathFX = forceKPath

    def to: KPath  = KPath(parent, ren.to  )

    // ---------------------------------------------------------------------------
    def actualOpt: Option[RPath] = if (isActual) Some(this) else None

    def pathOpt: Option[KPath] = if (isActual) Some(to ) else None
    def leafOpt: Option[Ren]   = if (isLeaf)   Some(ren) else None

    // ---------------------------------------------------------------------------
    def initPair: (Option[KPath], Ren) = (
      parent.in.noneIf(_.isEmpty).map(x => KPath(x.init, x.last)),
      ren)

    def initPair2: (Option[KPath], Either[Key, ActualRen]) = (
      parent.in.noneIf(_.isEmpty).map(x => KPath(x.init, x.last)),
      ren.either)

    // ---------------------------------------------------------------------------
    def tailPair : Either[Ren, (Key, RPath)] =
      parent match {
        case Nil => Left ( ren)
        case seq => Right((seq.head, RPath(seq.tail, ren))) }
      /*
        tailPair match {
          case Left ( renaming      ) => ???
          case Right((parent, rpath)) => ???
      */ }

  // ===========================================================================
  object RPath {
    def from(from: Key, to: Key): RPath = RPath(Nil, Ren(from, to))
    def from(path: KPathW)      : RPath = RPath(path.kpath.parent, Ren.from(path.kpath.key))

    // ---------------------------------------------------------------------------
    @deprecated("make sure ok") def from(path: Seq[Key], to: Key) = RPath(path.init /* TODO: safe? */, Ren(path.last, to)) /* from RenameDynamically */ }

// ===========================================================================
case class TKPath(path: KPath, typeNode: reflect.TypeNode) extends trgt.HasTypeNode with trgt.HasType {
  override val instantiatorOpt = None
  override def typeDuo         = trgt.TypeDuo.fromTypeNodeOnly(typeNode)

  // ---------------------------------------------------------------------------
  def fieldPair(c: Cls): (KPath, gallia.meta.Info) = (path, typeNode.forceNonBObjInfo)
  def isLeaf           : Boolean                   = path.isLeaf }

// ===========================================================================
case class OptionalKPath(value: Option[KPath]) {
    def appendLevel(key: Key): OptionalKPath =
      value
          .map(_.appendLevel(key))
          .orElse(Some(KPath.from(key)))
        .pipe(OptionalKPath.apply)

    def forcePath: KPath = value.force }

  // ===========================================================================
  object OptionalKPath {
    val Root = OptionalKPath(None) }

// ===========================================================================
case class OptionalRPath(value: Option[RPath]) { // TODO:?
    //def appendLevel(key: Key): OptionalRPath = ???
    //def forcePath: KPath = value.force
  }

  // ===========================================================================
  object OptionalRPath {
    val Root = OptionalRPath(None) }

// ===========================================================================
