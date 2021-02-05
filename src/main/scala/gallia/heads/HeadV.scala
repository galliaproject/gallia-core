package gallia.heads

import gallia._
import gallia.actions.ActionsOthers.MapV2V

// ===========================================================================
class HeadV[T: WTT /* will be Vle (Any) for data phase */] private[gallia] (
      override val nodeId : NodeId,
      override val handler: Handler)
    extends Head[HeadV[T]] {
  private[gallia] type Self = HeadV[T]

  override def toString: String = nodeId // TODO

  @deprecated("bypasses the 210205063004 mechanism") def forceValue: T = end().runv().forceData1b // TODO: or as forceData, forceResult?

  private[gallia] def rewrap[T2: WTT](newNodeId: NodeId): HeadV[T2] = new HeadV[T2](newNodeId, handler)

  // ===========================================================================
  def mapV [         T2: WTT](f: T  => T2)                            : HeadV[    T2 ] = handler.chainvv(this)(MapV2V(node[T2], (x: Any) => f(x.asInstanceOf[T]) ))
  def mapVs[T1: WTT, T2: WTT](f: T1 => T2)(implicit ev: T <:< Seq[T1]): HeadV[Seq[T2]] = mapV[Seq[T2]](_.map(f)) // worth keeping? - TODO: subclass rather?
}

// ===========================================================================
