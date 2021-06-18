package gallia.actions

import aptus.{Anything_, Seq_}
import aptus.Separator

import gallia._
import gallia.target._
import gallia.atoms.AtomsUUSomewhatBasics._
import gallia.atoms.AtomsUUResnesting

// ===========================================================================
object ActionsUUNestingRelated {
  import gallia.actions.utils.ActionsUUUtils.potentialRenaming

  // ===========================================================================
  class NestUnder(target: RPathz, destination: Key) extends ActionUUa {
        def  vldt(c: Cls): Errs = Nil //TODO
           // TODO: t210128155944 - validate new key names
           // TODO: check target is distinct
        def _meta(c: Cls): Cls  = target.values.map(_.renFX).thn(Renz.apply).thn(c.nest(_, destination))
        def atomuus =
          _Nest(target.fromz.forceKeyz /*FIXME*/, destination) +:
            target.map(_.prepend(destination)).flatMap(potentialRenaming).toSeq
    }

    // ---------------------------------------------------------------------------
    class NestInto(target: RPathz, destination: Ren) extends ActionUUa {      
        def  vldt(c: Cls): Errs = Nil //TODO
        def _meta(c: Cls): Cls  = target.values.map(_.renFX).thn(Renz.apply).thn(c.nest(_, destination.from)).rename(destination)
        def atomuus =
          (_Nest(target.fromz.forceKeyz /*FIXME*/, destination.from) +:
            target.map(_.prepend(destination.from)).flatMap(potentialRenaming).toSeq) ++ 
            potentialRenaming(destination)          
    }

  // ===========================================================================
  // TODO: split up

  case class UnnestFrom(parent: KPath, target: TQRenz) extends ActionUUc { //TODO: multiple levels? mark here if one, all, ...
    private def keyz(nc: Cls): Keyz = target.resolve(nc).fromsFX

    // ---------------------------------------------------------------------------
    def  vldt (c: Cls): Errs = {
      _vldt.checkNotNesting(c, parent) match {
        case Some(shortcutting) => shortcutting.as.seq
        case None =>
          val nc = c.forceNestedClass(parent)

          if (c.field(parent).isMultiple && nc.size > 1) errs("TODO:DoubleMultiple:210109145954")
          else target.vldtAsOrigin(nc) }} //TODO: more

    // ---------------------------------------------------------------------------
    def _meta (c: Cls): Cls = {
      val nc = c.forceNestedClass(parent)

      if (c.field(parent).isMultiple)
        keyz(nc)
          .force.one
          .thn { sole =>
            c .unnestField(parent, sole)
              .toMultiple(sole) }
      else
        keyz(nc)
          .foldLeft(c)(
              _.unnestField(parent, _))
    }

    // ---------------------------------------------------------------------------
    def atomuu(c: Cls) = {
      val nc = c.forceNestedClass(parent)
      val keyz = this.keyz(nc)

      // ---------------------------------------------------------------------------
           if (c.field(parent).isMultiple) _UnnestOOO (parent, keyz.values.force.one)
      else if (keyz.size == nc.size)       _UnnestAll (parent) // TODO: still worth handling separately?
      else                                 _UnnestSome(parent, keyz)
    }
  }

  // ===========================================================================
  case class Renest(targets: TqKeyz, sep: Separator) extends ActionUUc with TodoV1 {
      // TODO: check for potential resulting conflicts
      def _meta (c: Cls): Cls = targets.resolve(c).thn(AtomsUUResnesting.meta(_, sep)(c))
      def atomuu(c: Cls)      = targets.resolve(c).thn(_Renest(_, sep)) }

    // ---------------------------------------------------------------------------
    case class _Renest(targetKeys: Keyz, sep: Separator) extends AtomUU { def naive(o: Obj) =
      AtomsUUResnesting.data(targetKeys, sep)(o) }

  // ===========================================================================
  @deprecated("WIP: t210109144926 - nesting-related generalization") class Move(target: RPathz, destinationOpt: Option[RPath]) extends ActionUUa {
      def  vldt(c: Cls): Errs = Nil //TODO
      def _meta(c: Cls): Cls  = ???
      def atomuus = {

        val from = target.values.map(_.from).thn(KPathz.apply)
        _Move(
            from.values.force.one,
            destinationOpt.map(_.forceKPath)
          ).as.seq
      }
  }

}

// ===========================================================================
