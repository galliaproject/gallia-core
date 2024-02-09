package gallia
package env

import dag._

// ===========================================================================
case class ActionNodePair(id: NodeId, actionvm: ActionVMN)
      extends HasNodeId
      with    HasNoNodeContext
      with    HasNodeTarget[ActionVMN] {
    protected val target = actionvm

    // ---------------------------------------------------------------------------
    def tranform(f: ActionVMN => ActionVMN): ActionNodePair = copy(actionvm = f(actionvm))

    // ---------------------------------------------------------------------------
    def isNestingMetaPlaceholder: Boolean = actionvm.isInstanceOf[heads.HeadsNestingHandler.NestingMetaPlaceholder]
    def isActionXO: Boolean =
      actionvm.isInstanceOf[ActionUO] ||
      actionvm.isInstanceOf[ActionZO]

    // ---------------------------------------------------------------------------
    def intermediateMetaResultNode(data: Map[NodeId, run.ResultSchema]) =
      new run.IntermediateMetaResult.Node(
        id,
        origin  = actionvm.callSite,
        actiona = actionvm.pipe(ActionNodePair.actionAN),
        result  = id.pipe(data)) }

  // ===========================================================================
  object ActionNodePair {
    private def actionAN(actionvm: ActionVMN): ActionAN = // 210205060908
      actionvm match {
        case x: ActionAN                     => x // <=> asInstanceOf[ActionAN]
        case _: actions.in.InMemoryMetaInput => NestingDataPlaceholder // TODO: build-in InMemoryMetaInput
        case x                               => aptus.illegalState(s"not an ActionAN: ${x}") } }

// ===========================================================================