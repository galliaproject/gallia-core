package gallia.plans

import aptus.Anything_

import gallia._
import gallia.run._
import gallia.dag._
import gallia.env.ActionDag
import gallia.actions.in.InMemoryMetaInput
import gallia.heads.HeadsNestingHandler.NestingMetaPlaceholder

// ===========================================================================
case class MetaPlan(dag: ActionDag) { // requires slightly larger glasses
  def runMeta(rootId : RootId, input: Cls): IntermediateMetaResult =
    addMetaInput(rootId , input ).dag.pipe(IntermediatePlanPopulator.apply).run()

  // ---------------------------------------------------------------------------
  def runMeta(rootId1: RootId, rootId2: RootId, input1: Cls, input2: Cls): IntermediateMetaResult =
    addMetaInput(rootId1, input1).addMetaInput(rootId2, input2).dag.pipe(IntermediatePlanPopulator.apply).run()

  // ---------------------------------------------------------------------------
  private def addMetaInput(rootId: RootId, c: Cls): MetaPlan =
    dag
      .assert(
          _.lookup(rootId)._2.isInstanceOf[NestingMetaPlaceholder],
          _.lookup(rootId))
      .replaceNode(rootId -> InMemoryMetaInput(c))
      .pipe(MetaPlan.apply)
}

// ===========================================================================