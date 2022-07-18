package gallia
package atoms

import heads.merging.MergingData._

// ===========================================================================
object AtomsZZMerging {

  case object _UnionZZ extends AtomZZtoZ { def naive(z1: Objs, z2: Objs) = z1.union(z2) }
  case object _ZipZZ   extends AtomZZtoZ { def naive(z1: Objs, z2: Objs) = z1.zip  (z2) }

  // ===========================================================================
  case class _MergeObjsVle(f: (Objs, Vle) => Objs) extends AtomZVtoZ { def naive(z: Objs, v: Vle) = f(z, v) }
  // TODO: t220706093840 - other common combinations

  // ===========================================================================
  case class _Bring(targetKeys: Keyz, joinKeys: JoinKey) extends AtomZZtoZ { def naive(z1: Objs, z2: Objs) =
      z2.flatMap {
          _.retainOpt(targetKeys.prepend(joinKeys.right)) }
        .pipe(z1.join(JoinType.left, joinKeys)) }

    // ---------------------------------------------------------------------------
    case class _Join(joinType: JoinType, joinKeys: JoinKey) extends AtomZZtoZ { def naive(z1: Objs, z2: Objs) =
      z1.join(joinType, joinKeys)(z2) }

    // ---------------------------------------------------------------------------
    case class _CoGroup(joinType: JoinType, joinKeys: JoinKey, as: AsKeys) extends AtomZZtoZ { def naive(z1: Objs, z2: Objs) =
      z1.coGroup(joinType, joinKeys, as)(z2) }

}

// ===========================================================================
