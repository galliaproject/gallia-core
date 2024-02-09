package gallia
package plans

import aptus.Anything_

import dag.RootId
import run._

// ===========================================================================
/** most for addMetaInput */ // TODO: just reuse InitialMetaPlan?
case class NestedMetaPlan(dag: ActionMetaDag) { // requires slightly larger glasses
  def runMeta(rootId : RootId, input: Cls): IntermediateMetaPlan =
    addMetaInput(rootId , input ).dag.pipe(new InitialMetaPlan(_)).run()

  // ---------------------------------------------------------------------------
  def runMeta(rootId1: RootId, rootId2: RootId, input1: Cls, input2: Cls): IntermediateMetaPlan =
    addMetaInput(rootId1, input1).addMetaInput(rootId2, input2).dag.pipe(new InitialMetaPlan(_)).run()

  // ---------------------------------------------------------------------------
  private def addMetaInput(rootId: RootId, c: Cls): NestedMetaPlan =
    dag
      .assert(
          _.lookup(rootId).isNestingMetaPlaceholder,
          _.lookup(rootId))
      .replaceNode(ActionMetaNode(rootId, actions.in.InMemoryMetaInput(c)))
      .pipe(NestedMetaPlan.apply) }

// ===========================================================================