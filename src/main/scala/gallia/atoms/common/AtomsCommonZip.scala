package gallia
package atoms
package common

import aptus.String_
import aptus.Separator
import aptus.Nes

import atoms.common.AtomsCommonSomewhatBasics._Split

// ===========================================================================
object AtomsCommonZip {
  // TODO: if any are missing? ensure add can handle None (not adding)

  // ---------------------------------------------------------------------------
  def zip(o: Obj, keys: Renz, sep: Separator, newNestingKey: Key): Obj =
    keys
      .froms
      .map { key => _Split(key, _.splitBy(sep)) }
        .foldLeft(o)((curr, atom) => atom.naive(curr))
      .pipe { splitted =>
        splitted.add(
          newNestingKey,
          objs(splitted, keys)) }
      .remove(keys.froms)

  // ===========================================================================
  private def objs(splitted: Obj, keys: Renz): Seq[Obj] =
    valuess(splitted, keys.froms)
      .pipe(preTranspose)
      .transpose
      .map(keys.tos.values.zip(_))
      .map(obj)

    // ===========================================================================
    private def valuess(splitted: Obj, keys: Keyz): Nes[Seq[Any]] =
      keys
        .values
        .map { key =>
          splitted
            .opt(key)
            .map(_.asInstanceOf[Seq[_]] /* TODO: validated? */)
            .getOrElse(Nil) }

  // ===========================================================================
  private def preTranspose(valuess: Seq[Seq[AnyValue]]): Seq[Seq[AnyValue]] = {
    val sizes = valuess.map(_.size)
    val max = sizes.max

    //TODO: what if max is 0 or 1?

    require( // TODO: err:x
      sizes.toSet.diff(Set(0, 1, max)).isEmpty,
      (sizes, valuess))

    valuess
      .map { values =>
        values.size match {
          case 0 => Seq.fill(max)(None)
          case 1 => Seq.fill(max)(values.head)
          case _ => values } }
  }

}

// ===========================================================================

