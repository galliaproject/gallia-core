package gallia
package atoms

import aptus.{String_, Seq_}
import aptus.Separator

// ===========================================================================
object AtomsUUResnesting {

  def meta(targetKeys: Keyz, sep: Separator)(value: Cls): Cls =
      targetKeys
        .foldLeft(value) { (curr, key) =>
          metaRec(sep)(
              curr,
              formattedKey = key,
              keyItems     = key.name.splitBy(sep)) }

    // ---------------------------------------------------------------------------
    private def metaRec(sep: Separator)(o: Cls, formattedKey: Key, keyItems: Seq[SKey]): Cls =
      keyItems match {
        case Seq(one) => o // eg just baz

        case headSkey +: tailKeyItems =>
          val headKey = headSkey.symbol
          val formattedTail = tailKeyItems.join(sep) /* quite wasteful... */.symbol

          o .nest( // eg nest(foo_bar_baz ~> bar_baz, foo)
              formattedKey ~> formattedTail,
                headKey)

            .transformSoleInfo(headKey) { // best to recurse *after* nesting to avoid key collisions
              info =>
                info
                  .forceNestedClass // since we just nested it
                  .pipe(metaRec(sep)(_, formattedTail, tailKeyItems))
                  .pipe(info.updateContainee) } }

  // ===========================================================================
  def data(targetKeys: Keyz, sep: Separator)(value: Obj): Obj =
      value
        .entries
        .filter(x => targetKeys.contains(x._1)) // TODO: use Set
        // TODO: opt: filter only the ones that contain sep? benchmark
        .foldLeft(value) { (curr, entry) =>
          val (key, _) = entry

          dataRec(sep)(
              curr,
              formattedKey = key,
              keyItems = key.name.splitBy(sep)) }

    // ---------------------------------------------------------------------------
    private def dataRec(sep: Separator)(o: Obj, formattedKey: Key, keyItems: Seq[SKey]): Obj =
      keyItems match {
        case Seq(one) => o // eg one: baz

        case headKey +: tailKeyItems =>
          val formattedTail = tailKeyItems.join(sep) /* quite wasteful... */.symbol

          o
            // eg nest(foo_bar_baz ~> bar_baz).as(foo)
            .nest(formattedKey, headKey.symbol)
            .transformPath(headKey.symbol,
              // best to recurse *after* nesting to avoid key collisions
              _ .asInstanceOf[Obj]
                .rename(formattedKey ~> formattedTail)
                .pipe(dataRec(sep)(_, formattedTail, tailKeyItems)) ) }

}

// ===========================================================================

