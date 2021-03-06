package gallia.run

import aptus.Anything_

import gallia._
import gallia.actions.in.InMemoryMetaInput

// ===========================================================================
object IntermediateMetaResultNodeCreator {

  def apply(asg: gallia.env.ActionDag)(data: Map[NodeId, ResultSchema]): IntermediateMetaResult =
    asg
      .transform { case (id, action) =>
          IntermediateMetaResultNode(
              id,
              origin = action.callSite,
              action = actionAN(action),
              result = data(id)) }(
        newIdResolver = _.id)
      .thn(IntermediateMetaResult.apply)

  // ===========================================================================
  private[gallia] def actionAN(action: ActionVMN): ActionAN = // 210205060908
    action match {
      case x: ActionAN          => x // <=> asInstanceOf[ActionAN]
      case x: InMemoryMetaInput => NestingDataPlaceholder // TODO: build-in InMemoryMetaInput
      case x                    => illegal(s"${x}") }

}

// ===========================================================================