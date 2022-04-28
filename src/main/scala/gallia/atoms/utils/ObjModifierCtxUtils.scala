package gallia
package atoms
package utils

// ===========================================================================
object ObjModifierCtxUtils {

  // TODO: t220422111733 - try and guess based on keys (unsupported yet)
  def guessOpt(nestings: Seq[ObjModifierCtx])(o: Obj): Option[ObjModifierCtx] = {
    val keySet: Set[Key] = o.keySet

    val counts: Seq[(ObjModifierCtx, aptus.Count)] =
      nestings
        .map { ctx => ctx ->
          ctx.keySet.intersect(keySet).size }

    counts
      .map(_._2)
      .max
      .pipe { maxCommonKeys =>
        counts.ifOneMatch(_._2 == maxCommonKeys)(
          sole => Some(sole._1),
          more => { println(more.size); ??? }) } // TODO: error message (guessing failed)
  }

}

// ===========================================================================