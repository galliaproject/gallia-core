package gallia
package plans

import aptus.Anything_

import run._
import dag._
import env.ActionDag
import actions.in.InMemoryMetaInput

// ===========================================================================
/** most for addMetaInput */ // TODO: just reuse IntermediatePlan?
case class NestedMetaPlan(dag: ActionDag) { // requires slightly larger glasses
  def runMeta(rootId : RootId, input: Cls): IntermediateMetaResult =
    addMetaInput(rootId , input ).dag.pipe(new IntermediatePlan(_)).run()

  // ---------------------------------------------------------------------------
  def runMeta(rootId1: RootId, rootId2: RootId, input1: Cls, input2: Cls): IntermediateMetaResult =
    addMetaInput(rootId1, input1).addMetaInput(rootId2, input2).dag.pipe(new IntermediatePlan(_)).run()

  // ---------------------------------------------------------------------------
  private def addMetaInput(rootId: RootId, c: Cls): NestedMetaPlan =
    dag
      .assert(
          _.lookup(rootId).isNestingMetaPlaceholder,
          _.lookup(rootId))
      .replaceNode(env.ActionNodePair(rootId, InMemoryMetaInput(c)))
      .pipe(NestedMetaPlan.apply) }

// ===========================================================================