package gallia
package env

import dag.NodeId

// ===========================================================================
case class ActionNodePair(id: NodeId, actionvm: ActionVN with ActionMN) {
    def tranform(f: ActionVN with ActionMN => ActionVN with ActionMN): ActionNodePair = copy(actionvm = f(actionvm))

    // ---------------------------------------------------------------------------
    def isNestingMetaPlaceholder: Boolean = actionvm.isInstanceOf[heads.HeadsNestingHandler.NestingMetaPlaceholder]
    def isActionXO: Boolean =
      actionvm.isInstanceOf[ActionUO] ||
      actionvm.isInstanceOf[ActionZO]

    // ---------------------------------------------------------------------------
    def intermediateMetaResultNode(data: Map[NodeId, run.ResultSchema]) =
      new run.IntermediateMetaResultNode(
        id,
        origin   = actionvm.callSite,
        actionan = actionvm.pipe(ActionNodePair.actionAN),
        result   = id.pipe(data)) }

  // ===========================================================================
  object ActionNodePair {
    private def actionAN(action: ActionVN with ActionMN): ActionAN = // 210205060908
      action match {
        case x: ActionAN                     => x // <=> asInstanceOf[ActionAN]
        case _: actions.in.InMemoryMetaInput => NestingDataPlaceholder // TODO: build-in InMemoryMetaInput
        case x                               => aptus.illegalState(s"not an ActionAN: ${x}") } }

// ===========================================================================