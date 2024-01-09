package gallia
package plans

import aptus.Seq_

import dag.HasNodeId

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
  def atomNode(newNodeId: NodeId, atom: Atom) = 
    AtomNode(
        newNodeId,
        atom,
        AtomNodeDebugging(
            parentId = id,
            ctx,
            origin))

}

// ===========================================================================
