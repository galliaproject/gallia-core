package gallia
package plans

import aptus.Seq_

import dag._

// ===========================================================================
case class ActionNode(
      id    : NodeId,
      ctx   : ActionMetaContext,
      atoms : Seq[Atom] /* may be empty, eg for ValidateX or OutputX */)
    extends HasNodeId
    with    HasNodeContext[ActionMetaContext]
    with    HasNodeTarget [Seq[Atom]] {
  protected val ctxOpt = Some(ctx)
  protected val target = atoms

  // ---------------------------------------------------------------------------
  override def toString: String = formatDefault

    def formatDefault: String =
      atoms match {
        case Nil       => s"no-atoms:${ctx.origin.formatDefault}"
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
        ctx = AtomMetaContext(
          actionId      = id,
          actionMetaCtx = ctx),
        atom)

    // ---------------------------------------------------------------------------
    private[plans] def atomNodeIds: Seq[NodeId] =
      atoms
        .ensuring(_.nonEmpty)
        .zipWithIndex
        .map { case (_, index) =>
          atomId(id, index) } }

// ===========================================================================
