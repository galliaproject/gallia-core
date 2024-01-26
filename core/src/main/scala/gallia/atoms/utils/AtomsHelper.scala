package gallia
package atoms
package utils

import aptus.Int_

// ===========================================================================
/** to help declutter the dedicated ones */
object AtomsHelper {

  @Distributivity
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
    o .keys
      .pipe(f)
      .map { key =>
        key ->
          (o.forceKey(key) match { // TODO: t210115095838 - optimization: pass nesting info from meta rather
            case x: Obj    => reorderKeysRecursively(f)(x)
            case x: Seq[_] =>
              x.map {
                case y: Obj    => reorderKeysRecursively(f)(y)
                case y: Seq[_] => ???//TODO: can't happen throw illegal
                case y         => y }
            case x => x }) }
      .pipe(gallia.obj)

  // ===========================================================================
  def unnestOOO (o: Obj, parent: KPath, key: Key  ): Obj = AtomsUtils.nestingx(o, parent)(ifLeaf = unnestOOOLeaf (_, _, key ))(ifNesting = unnestOOO (_, _, key ))
  def unnestAll (o: Obj, parent: KPath            ): Obj = AtomsUtils.nestingx(o, parent)(ifLeaf = unnestAllLeaf (_, _      ))(ifNesting = unnestAll (_, _      ))
  def unnestSome(o: Obj, parent: KPath, keyz: Keyz): Obj = AtomsUtils.nestingx(o, parent)(ifLeaf = unnestSomeLeaf(_, _, keyz))(ifNesting = unnestSome(_, _, keyz))

    // ---------------------------------------------------------------------------
    def unnestOOOLeaf(o: Obj, parent: Key, key: Key): Obj =
      o .objs_(parent)
        .map { _.map(_.forceKey(key)) }
        .map { values =>
          o .removeOpt(parent)
            .map       { _.putEntry(key,   values) }
            .getOrElse { obj       (key -> values) } }
        .getOrElse(o)

    // ---------------------------------------------------------------------------
    def unnestAllLeaf(o: Obj, parent: Key): Obj =
      o .obj_(parent)
        .map { o2 =>
          o .removeOpt(parent)
            .map(_.merge(o2))
            .getOrElse  (o2) }
        .getOrElse(o)

    // ---------------------------------------------------------------------------
    def unnestSomeLeaf(o: Obj, parent: Key, keyz: Keyz): Obj =
      o .obj_(parent)
        .map { o2 =>
          o2.retainOpt(keyz)
            .map(o.merge)
            .map(_.transformObj(parent, _.remove(keyz)))
            .getOrElse(o) }
        .getOrElse(o)

}

// ===========================================================================
