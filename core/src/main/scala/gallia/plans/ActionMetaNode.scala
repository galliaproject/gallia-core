package gallia
package plans

import dag._

// ===========================================================================
case class ActionMetaNode(id: NodeId, actionvm: ActionVMN)
      extends HasNodeId
      with    HasNoNodeContext
      with    HasNodeTarget[ActionVMN] {
    protected val target = actionvm

    // ---------------------------------------------------------------------------
    def tranform(f: ActionVMN => ActionVMN): ActionMetaNode = copy(actionvm = f(actionvm))

    // ---------------------------------------------------------------------------
    def isNestingMetaPlaceholder: Boolean = actionvm.isInstanceOf[heads.HeadsNestingHandler.NestingMetaPlaceholder]
    def isActionXO: Boolean =
      actionvm.isInstanceOf[ActionUO] ||
      actionvm.isInstanceOf[ActionZO]

    // ---------------------------------------------------------------------------
    def intermediateMetaPlanNode(data: Map[NodeId, run.ResultSchema]) =
      new run.IntermediateMetaPlan.Node(
        id,
        origin  = actionvm.callSite,
        actiona = actionvm.pipe(ActionMetaNode.actionAN),
        result  = id.pipe(data)) }

  // ===========================================================================
  object ActionMetaNode {
    private def actionAN(actionvm: ActionVMN): ActionAN = // 210205060908
      actionvm match {
        case x: ActionAN                     => x // <=> asInstanceOf[ActionAN]
        case _: actions.in.InMemoryMetaInput => NestingDataPlaceholder // TODO: build-in InMemoryMetaInput
        case x                               => aptus.illegalState(s"not an ActionAN: ${x}") } }

// ===========================================================================