package gallia
package run

import actions.in.InMemoryMetaInput

// ===========================================================================
object IntermediateMetaResultNodeCreator {

  def apply(asg: gallia.env.ActionDag)(data: Map[NodeId, ResultSchema]): IntermediateMetaResult =
    asg
      .transform { x =>
          IntermediateMetaResultNode(
              x.id,
              origin   = x.actionvmn.callSite,
              actionan = x.actionvmn.pipe(actionAN),
              result   = x.id.pipe(data)) }(
        newIdResolver = _.id)
      .pipe(IntermediateMetaResult.apply)

  // ===========================================================================
  private[gallia] def actionAN(action: ActionVN with ActionMN): ActionAN = // 210205060908
    action match {
      case x: ActionAN          => x // <=> asInstanceOf[ActionAN]
      case _: InMemoryMetaInput => NestingDataPlaceholder // TODO: build-in InMemoryMetaInput
      case x                    => aptus.illegalState(s"not an ActionAN: ${x}") }

}

// ===========================================================================
