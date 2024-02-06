package gallia
package oswo

import aptus._

// ===========================================================================
trait OswoPrototype {
  private[gallia] type C2C = Cls => Cls

  // ---------------------------------------------------------------------------
  private var oswoPrototype = false

    def withEnabledOswoPrototype[T](f: => T): T = {
      oswoPrototype = true; val result = f; oswoPrototype = false

      result }

    private[gallia] def isOswoPrototypeEnabled: Boolean = oswoPrototype

  // ===========================================================================
  def __exampleOswoPrototypeUsage(): Any /* result case class */ = {
    gallia.withEnabledOswoPrototype {
      bobj("f" -> "foo", "g" -> 1)
          .transform(_.string("f" ~> "F")).using(_.toUpperCase)
          .rename   ("g" ~> "G")
          .increment("G")
          .add      ("h" -> true)
          .replace  ("h" -> false)
          .convert  ("G").toDouble
          .remove   ("h")
        .oswo() } // .toString => "cc_7(FOO,2.0)"
  }

  // ===========================================================================
  private[oswo] var finalCode: String = ""
                val serials = collection.mutable.Map[String, Array[Byte]]() // TODO: very insecure as is...

  // ---------------------------------------------------------------------------
  private[gallia] def oswoRun(head: heads.Head[_]) = OswoRun(head)

  // ===========================================================================
  private[gallia] case class IntraActionClss(values: Seq[Cls]) { import aptus._ // includes init or not? i think not
    values.requireNonEmpty()

    def tailSize = values.size - 1
    def tail     = values.tail
    def slidingPairs: Seq[(Cls, Cls)] = values.slidingPairs }

  // ===========================================================================
  private[gallia] implicit class OswoRPathz_(rpaths: RPathz) { import aptus._

    def intraClss(c: Cls)(f: (Cls, KPath) => Cls): Seq[Cls] =
      rpaths
        .scanLeft(c.in.seq) { (cs, rpath) =>
          val renamingOpt: Option[C2C] = rpath.actualOpt.map(x => _.rename(x))
          val specific   :        C2C  = f(_, rpath.to)

          cs.last.scanLeft(renamingOpt.toList :+ specific) }
        .flatten
        .tail }

  // ===========================================================================
  private[gallia] implicit class OswoCls_(c: Cls) {
    import OswoAtomFormatterUtils._

    // ---------------------------------------------------------------------------
    def formatSourceLines(nodeId: ANI)(name: String): Seq[SourceString] =
      source.CaseClassHierarchyFormatting
        .apply(name = nodeId.cc(name))(c)
        .map  (_.formatSource)

    // ---------------------------------------------------------------------------
    def scanLeft(fs: Seq[C2C]): Seq[Cls] = fs.scanLeft(c) { (c2, f) => f.apply(c2) } }

  // ===========================================================================
  private[gallia] implicit class OswoBasicType_(basicType: BasicType) {
    def formatBasicType(key: Key)(o: Obj): SourceString = {
      basicType match {
        //TODO: container
        case meta.BasicType._String  => o.string (key).quote
        case meta.BasicType._Boolean => o.boolean(key).pipe(data.DataFormatting.formatBoolean)
        case meta.BasicType._Int     => o.int    (key).pipe(data.DataFormatting.formatInt) } }

    // ---------------------------------------------------------------------------
    def formatBasicType(value: AnyValue): SourceString = {
      basicType match {
        //TODO: container
        case meta.BasicType._String  => value.asInstanceOf[String] .quote
        case meta.BasicType._Boolean => value.asInstanceOf[Boolean].pipe(data.DataFormatting.formatBoolean)
        case meta.BasicType._Int     => value.asInstanceOf[Int]    .pipe(data.DataFormatting.formatInt) } } }

  // ===========================================================================
  private[gallia] implicit class OswoAtoms_(_atoms: Seq[AtomOswo]) {

    def updateAtomMetas(_intras: IntraActionClss): Unit = {
      assert(
        _intras.tailSize == _atoms.size,
        _intras.tailSize -> _atoms.size ->
        _intras.tail.map(_.formatDefault).joinln.newline -> _atoms.joinln)

      // ---------------------------------------------------------------------------
      _intras
        .slidingPairs
        .zipSameSize(_atoms)
        .foreach { case ((in, out), atom) =>
          atom._metaIO = plans.ClsIO(in, out) } } }

  // ===========================================================================
  private[gallia] implicit class Atom___39(u: Atom) {
    // adds to finalCode
    def optim(id: ANI, fromIdOpt: Option[ANI])(last: IsLast)(efferent: Cls /* just for IX... */): Unit =
      OswoAtomMatcher(u)(id, fromIdOpt)(last)(efferent) }

}

// ===========================================================================
