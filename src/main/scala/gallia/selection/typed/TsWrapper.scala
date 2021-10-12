package gallia
package selection.typed

import selection.untyped.processors._

// ===========================================================================
class TsWrapper[T] private[gallia] (
        selection      : Any /* TODO: wrap */,
    val ignoreContainer: Boolean = false /* eg for stringx('foo); TODO: if true must be One? */) {
    // TODO: add Whatever flag (easier, also see 210122154401)

  // ===========================================================================
  private def _tqkpath (value: KPath)  = new TqKPath (_ => Nil, _ => value)

    // ---------------------------------------------------------------------------
    def forceTqKPath: TqKPath =
      selection match {
        case x: KeyW => _tqkpath(x.kpath)
        case KPathW(x) => _tqkpath(x)
        case RPathW(x) => _tqkpath(x.forceKPath) // FIXME: restrict, eg filterBy only use to use non-renaming

        // ---------------------------------------------------------------------------
        case x: KPathSelection => x.tqkpath }

  // ===========================================================================
  private def _tqqpathz(value: RPathW) = new TqRPathz(_ => Nil, _ => value.qpathz)
  private def _tqqpathz(value: RPathz) = new TqRPathz(_ => Nil, _ => value)

    // ---------------------------------------------------------------------------
    def forceTqRPathz: TqRPathz =
      selection match {
        case x: KeyW    => _tqqpathz(x.value)
        case x: RenW    => _tqqpathz(x.value)
        case x: KPathW  => _tqqpathz(x.value)
        case x: RPathW  => _tqqpathz(x.value)

        // ---------------------------------------------------------------------------
        case x: RPathz => _tqqpathz(x) // from RemoveConditionally...

        case x: Tuple3[_, _, _] => // TODO: for?
          x._1 match {
            case _: RPathW => _tqqpathz(qpathz(x)) }

        case x: RPathzSelection => x.tqqpathz
      }

  // ===========================================================================
//private def kpath (x: Tuple3[_, _, _]): KPath  = ((x._1.asInstanceOf[KPathW], x._2.asInstanceOf[KPathW], x._3.asInstanceOf[Seq[KPathW]]): KPathWz).kpath
  private def qpathz(x: Tuple3[_, _, _]): RPathz = ((x._1.asInstanceOf[RPathW], x._2.asInstanceOf[RPathW], x._3.asInstanceOf[Seq[RPathW]]): RPathWz).qpathz
}

// ===========================================================================
