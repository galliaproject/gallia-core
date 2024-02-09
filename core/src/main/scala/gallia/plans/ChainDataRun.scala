package gallia
package plans

import dag._

// ===========================================================================
object ChainDataRun {

  def apply(missingInputs: Map[RootId, NDT])(atomNodes: ChainAtomNodes): NDT = {
    var latest: NDT = null // using foldLeft is tricky because of missingInputs

    atomNodes
      .valuesWithContext
      .foreach { case (fromIdOpt, atomNode, isLast) =>
        if (gallia.isOswoPrototypeEnabled)
          atomNode.atom.optim(
            atomNode.id, fromIdOpt)(isLast)(
            atomNode.debug.ctx.efferent /* remove eventually */)

        // ---------------------------------------------------------------------------
        latest =
          atomNode.process(
            input = DataInput.SingleInput(latest))(
            missingInputs.apply) }

    latest } }

// ===========================================================================
