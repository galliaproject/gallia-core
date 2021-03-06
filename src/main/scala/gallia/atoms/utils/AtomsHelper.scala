package gallia.atoms.utils

import aptus.Anything_
import aptus.Int_

import gallia.obj
import gallia._

// ===========================================================================
/** to help declutter the dedicated ones */
object AtomsHelper {

  @gallia.Distributivity
  def logProgress(dis: Objs, nOpt: Option[Int], debug: Obj => String): Objs =
    nOpt
      .map { n =>
        var count = 0
        dis
          .map { line =>
            count += 1
            if ((count % n) == 0) println(s"\t${count.formatUsLocale}\t${debug(line)}")

            line } }
      .getOrElse(dis)

  // ===========================================================================
  def reorderKeysRecursively(f: Seq[Key] => Seq[Key])(o: Obj): Obj =
    f(o.keys)
      .map { key =>
        key ->
          (o.force(key) match { // TODO: t210115095838 - optimization: pass nesting from meta rather
            case x: Obj    => reorderKeysRecursively(f)(x)
            case x: Seq[_] =>
              x.map {
                case y: Obj    => reorderKeysRecursively(f)(y)
                case y: Seq[_] => ???//TODO: can't happen throw illegal
                case y         => y }
            case x => x }) }
      .thn(gallia.obj)

  // ===========================================================================
  def unnestOOO (o: Obj, parent: KPath, key: Key  ): Obj = AtomsUtils.nestingx(o, parent)(unnestOOO (_, _, key ))(unnestOOO (_, _, key ))
  def unnestAll (o: Obj, parent: KPath            ): Obj = AtomsUtils.nestingx(o, parent)(unnestAll (_, _      ))(unnestAll (_, _      ))
  def unnestSome(o: Obj, parent: KPath, keyz: Keyz): Obj = AtomsUtils.nestingx(o, parent)(unnestSome(_, _, keyz))(unnestSome(_, _, keyz))

    // ---------------------------------------------------------------------------
    def unnestOOO(o: Obj, parent: Key, key: Key): Obj =
      o .objs_(parent)
        .map(_.map(_.force(key)))
        .map { values =>
          o .removeOpt(parent)
            .map(_    .put(key,   values))
            .getOrElse(obj(key -> values)) }
        .getOrElse(o)

    // ---------------------------------------------------------------------------
    def unnestAll(o: Obj, parent: Key): Obj =
      o .obj_(parent)
        .map { o2 =>
          o .removeOpt(parent)
            .map(_.merge(o2))
            .getOrElse  (o2) }
        .getOrElse(o)

    // ---------------------------------------------------------------------------
    def unnestSome(o: Obj, parent: Key, keyz: Keyz): Obj =
      o .obj_(parent)
        .map { o2 =>
          o2.retainOpt(keyz)
            .map(o.merge)
            .map(_.transformObj(parent, _.remove(keyz)))
            .getOrElse(o2) }
        .getOrElse(o)

}

// ===========================================================================
