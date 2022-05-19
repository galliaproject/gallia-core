package gallia
package atoms
package utils

// ===========================================================================
object ObjModifierCtxUtils {

  // TODO: t220422111733 - try and guess based on keys (unsupported yet)
  def guess(nestings: Seq[ObjModifierCtx])(o: Obj): ObjModifierCtx = {
    val keySet: Set[Key] = o.keySet

    val counts: Seq[(ObjModifierCtx, aptus.Count)] =
      nestings
        .map { ctx => ctx ->
          ctx.keySet.intersect(keySet).size }

    counts
      .map(_._2)
      .max
      .pipe { maxCommonKeys => counts.filter(_._2 == maxCommonKeys) }
      .ifOneElementOrElse(errorMessage = xs => s"220519100740:Guessing failed: ${xs.size}") // TODO
      ._1
  }

}

// ===========================================================================