package gallia
package heads

import actions.ActionsOthers.{MapV2V, CombineVV}

// ===========================================================================
class HeadV[T: WTT /* will be Vle (Any) for data phase */] private[gallia] (
      override val nodeId : NodeId,
      override val handler: Handler)
    extends Head[HeadV[T]] 
    with    HeadVOut[T] {
  private[gallia] type Self = HeadV[T]
  private         val  self = this

  override def toString: String = nodeId // TODO

  @deprecated("bypasses the 210205063004 mechanism") def forceValue: T = end().runv().forceData1b // TODO: or as forceData, forceResult?

  private[gallia] def rewrap[T2: WTT](newNodeId: NodeId): HeadV[T2] = new HeadV[T2](newNodeId, handler)

  // ===========================================================================
  def mapV [         T2: WTT](f: T  => T2)                            : HeadV[    T2 ] = handler.chainvv(this)(MapV2V(typeNode[T2], (x: Any) => f(x.asInstanceOf[T]) ))
  def mapVs[T1: WTT, T2: WTT](f: T1 => T2)(implicit ev: T <:< Seq[T1]): HeadV[Seq[T2]] = mapV[Seq[T2]](_.map(f)) // worth keeping? - TODO: subclass rather?

  // ---------------------------------------------------------------------------
  def combine[T2: WTT, T3: WTT](that: HeadV[T2]) = new {
    def using(f: (T, T2) => T3): HeadV[T3] = handler.joinVv2v[T, T2, T3](self, that)( // TODO: as "reduce"?
      CombineVV(typeNode[T], typeNode[T2], (x: Any, y: Any) => f(x.asInstanceOf[T], y.asInstanceOf[T2]))) }
}

// ===========================================================================
