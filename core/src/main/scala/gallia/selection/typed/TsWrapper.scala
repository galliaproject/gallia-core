package gallia
package selection
package typed

import selection.untyped.processors._

// ===========================================================================
class TsWrapper[T] private[gallia] (
        selection      : Any /* TODO: wrap */,
    val ignoreContainer: Boolean = false /* eg for stringx('foo); TODO: if true must be One? */) {
    // TODO: add Whatever flag (easier, also see 210122154401)

  // ===========================================================================
  private def _tqkpath (value: KPath) = new TqKPath(_ => Nil, _ => value)
  private def _tqkey   (value: Key)   = new TqKey  (_ => Nil, _ => value)

    // ---------------------------------------------------------------------------
    def forceTqKey: TqKey = selection match { case x: KeyW => _tqkey(x.value) }

    // ---------------------------------------------------------------------------
    def forceTqKPath: TqKPath =
      selection match {
        case x: KeyW => _tqkpath(x.kpath)
        case KPathW(x) => _tqkpath(x)
        case RPathW(x) => _tqkpath(x.forceKPath) // FIXME: restrict, eg filterBy only use to use non-renaming

        // ---------------------------------------------------------------------------
        case x: KPathSelection => x.tqkpath }

  // ===========================================================================
  private def _tqrpathz(value: RPathW) = new TqRPathz(_ => Nil, _ => value.rpathz)
  private def _tqrpathz(value: RPathz) = new TqRPathz(_ => Nil, _ => value)

    // ---------------------------------------------------------------------------
    def forceTqRPathz: TqRPathz =
      selection match {
        case x: KeyW    => _tqrpathz(x.value)
        case x: RenW    => _tqrpathz(x.value)
        case x: KPathW  => _tqrpathz(x.value)
        case x: RPathW  => _tqrpathz(x.value)

        // ---------------------------------------------------------------------------
        case x: RPathz => _tqrpathz(x) // from RemoveConditionally...

        case x: Tuple3[_, _, _] => // TODO: for?
          x._1 match {
            case _: RPathW => _tqrpathz(rpathz(x)) }

        case x: RPathzSelection => x.tqrpathz
      }

  // ===========================================================================
//private def kpath (x: Tuple3[_, _, _]): KPath  = ((x._1.asInstanceOf[KPathW], x._2.asInstanceOf[KPathW], x._3.asInstanceOf[Seq[KPathW]]): KPathWz).kpath
  private def rpathz(x: Tuple3[_, _, _]): RPathz = ((x._1.asInstanceOf[RPathW], x._2.asInstanceOf[RPathW], x._3.asInstanceOf[Seq[RPathW]]): RPathWz).rpathz
}

// ===========================================================================
