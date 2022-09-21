package gallia
package actions
package common

import aptus.{Anything_, Seq_}
import aptus.Separator

import target._
import atoms.common.AtomsCommonSomewhatBasics._
import atoms.common.AtomsCommonResnesting

// ===========================================================================
object ActionsCommonNestingRelated {
  import gallia.actions.utils.ActionsUtils.potentialRenaming

  // ===========================================================================
  class NestUnder(target: TqRPathz, destination: Key) extends ActionUUb {
        def  vldt(c: Cls): Errs = target.vldtAsOrigin(c) ++ Nil //TODO
           // TODO: t210128155944 - validate new key names
           // TODO: check target is distinct
        def _meta(c: Cls): Cls  = target.resolve(c).map(_.renFX).pipe(Renz.apply).pipe(c.nest(_, destination))
        def atomuus(c: Cls)     = target.resolve(c).pipe { rpathz =>
          _Nest(rpathz.fromz.forceKeyz /*FIXME*/, destination) +:
            rpathz.map(_.prepend(destination)).flatMap(potentialRenaming(_).toSeq).toSeq }
    }

    // ---------------------------------------------------------------------------
    class NestInto(target: TqRPathz, destination: Ren) extends ActionUUb {      
        def  vldt(c: Cls): Errs = target.vldtAsOrigin(c) ++ Nil //TODO
        def _meta(c: Cls): Cls  = target.resolve(c).map(_.renFX).pipe(Renz.apply).pipe(c.nest(_, destination.from)).rename(destination)
        def atomuus(c: Cls)     = target.resolve(c).pipe { rpathz =>
          (_Nest(rpathz.fromz.forceKeyz /*FIXME*/, destination.from) +:
            rpathz.map(_.prepend(destination.from)).flatMap(potentialRenaming(_).toSeq).toSeq) ++
            potentialRenaming(destination) }
    }

  // ===========================================================================
  // TODO: split up

  case class UnnestFrom(parent: KPath, target: TqRenz) extends ActionUUb { //TODO: multiple levels? mark here if one, all, ...
    private def fromz(nc: Cls): Keyz = target.resolve(nc).froms

    // ---------------------------------------------------------------------------
    def  vldt (c: Cls): Errs = {
      _vldt.checkNotNesting(c, parent) match {
        case Some(shortcutting) => shortcutting.in.seq
        case None =>
          val nc = c.forceNestedClass(parent)

          if (c.field(parent).subInfo1.isMultiple && nc.size > 1) errs("TODO:DoubleMultiple:210109145954")
          else target.vldtAsOrigin(nc) }} //TODO: more (check renamings, ...)

    // ---------------------------------------------------------------------------
    def _meta (c: Cls): Cls = {
      val nc = c.forceNestedClass(parent)

      // TODO: split in two actions rather
      (  if (c.field(parent).subInfo1.isMultiple)
          fromz(nc)
            .force.one
            .pipe { sole =>
              c .unnestField(parent, sole)
                .toMultiple(sole) }
        else
          fromz(nc)
            .foldLeft(c)(
                _.unnestField(parent, _)))
      .rename(target.resolve(nc))
    }

    // ---------------------------------------------------------------------------
    def atomuus(c: Cls) = {
      val nc = c.forceNestedClass(parent)
      val fromz = this.fromz(nc)

      // ---------------------------------------------------------------------------
      (      if (c.field(parent).subInfo1.isMultiple) _UnnestOOO (parent, fromz.values.force.one)
        else if (fromz.size == nc.size)            _UnnestAll (parent) // TODO: still worth handling separately?
        else                                       _UnnestSome(parent, fromz)) +:
      target.resolve(nc).flatMap(potentialRenaming(_))
    }
  }

  // ===========================================================================
  case class Renest(targets: TqKeyz, sep: Separator) extends ActionUUc with TodoV1 {
      // TODO: check for potential resulting conflicts
      def _meta (c: Cls): Cls = targets.resolve(c).pipe(AtomsCommonResnesting.meta(_, sep)(c))
      def atomuu(c: Cls)      = targets.resolve(c).pipe(_Renest(_, sep)) }

    // ---------------------------------------------------------------------------
    case class _Renest(targetKeys: Keyz, sep: Separator) extends AtomUU { def naive(o: Obj) =
      AtomsCommonResnesting.data(targetKeys, sep)(o) }

  // ===========================================================================
  @deprecated("WIP: t210109144926 - nesting-related generalization") class Move(target: RPathz, destinationOpt: Option[RPath]) extends ActionUUa {
      def  vldt(c: Cls): Errs = Nil //TODO
      def _meta(c: Cls): Cls  = ???
      def atomuus = {

        val from = target.values.map(_.from).pipe(KPathz.apply)
        _Move(
            from.values.force.one,
            destinationOpt.map(_.forceKPath)
          ).in.seq
      }
  }

}

// ===========================================================================
