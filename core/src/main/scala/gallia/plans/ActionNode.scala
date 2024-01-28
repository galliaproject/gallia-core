package gallia
package plans

import aptus.Seq_

import dag.HasNodeId

// ===========================================================================
case class ActionNode(
      id    : NodeId,
      atoms : Seq[Atom], // may be empty, eg for ValidateX or OutputX
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
  def atomNodes: Seq[AtomNode] = {
    atomNodeIds
      .zip { atoms }
      .map { case (newNodeId, atom) =>
        atomNode(newNodeId, atom) } }

    // ---------------------------------------------------------------------------
    private def atomNode(newNodeId: NodeId, atom: Atom): AtomNode =
      AtomNode(
          newNodeId,
          atom,
          AtomNodeDebugging(
              parentId = id,
              ctx,
              origin))

    // ---------------------------------------------------------------------------
    private[plans] def atomNodeIds: Seq[NodeId] =
      atoms
        .ensuring(_.nonEmpty)
        .zipWithIndex
        .map { case (_, index) =>
          AtomNode.atomId(id, index) } }

// ===========================================================================
