package gallia.plans

import aptus.Seq_

import gallia.dag.HasNodeId
import gallia.dag.NodeId
import gallia._

// ===========================================================================
case class ActionNode(
      id    : NodeId,
      atoms : Seq[Atom],
      ctx   : NodeMetaContext,
      origin: CallSite)
    extends HasNodeId {

  override def toString: String = formatDefault

    def formatDefault: String =
      atoms match {
        case Nil       => s"no-atoms:${origin.formatDefault}"
        case Seq(sole) => sole.formatDefault
        case mult      => mult.zipWithRank.map { case (atom, rank) => ??? /*atom.formatDefault*/ }.join(", ") }

  // ---------------------------------------------------------------------------
  def atomNode(newNodeId: NodeId, atom: Atom) = AtomNode(newNodeId, atom, parentId = id, ctx, origin)
}

// ===========================================================================
case class AtomNode(
    id      : NodeId,
    atom    : Atom,
    parentId: NodeId, // ActionNode's
    ctx     : NodeMetaContext,
    origin  : CallSite)
  extends HasNodeId

// ===========================================================================