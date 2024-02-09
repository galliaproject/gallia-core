package gallia
package env

import dag.NodeId

// ===========================================================================
case class ActionNodePair(id: NodeId, actionvmn: ActionVN with ActionMN) {
    def tranform(f: ActionVN with ActionMN => ActionVN with ActionMN): ActionNodePair = copy(actionvmn = f(actionvmn))

    // ---------------------------------------------------------------------------
    def isNestingMetaPlaceholder: Boolean = actionvmn.isInstanceOf[heads.HeadsNestingHandler.NestingMetaPlaceholder]
    def isActionXO: Boolean =
      actionvmn.isInstanceOf[ActionUO] ||
      actionvmn.isInstanceOf[ActionZO]

    // ---------------------------------------------------------------------------
    def intermediateMetaResultNode(data: Map[NodeId, run.ResultSchema]) =
      run.IntermediateMetaResultNode(
        id,
        origin   = actionvmn.callSite,
        actionan = actionvmn.pipe(ActionNodePair.actionAN),
        result   = id.pipe(data)) }

  // ===========================================================================
  object ActionNodePair {
    private def actionAN(action: ActionVN with ActionMN): ActionAN = // 210205060908
      action match {
        case x: ActionAN                     => x // <=> asInstanceOf[ActionAN]
        case _: actions.in.InMemoryMetaInput => NestingDataPlaceholder // TODO: build-in InMemoryMetaInput
        case x                               => aptus.illegalState(s"not an ActionAN: ${x}") } }

// ===========================================================================