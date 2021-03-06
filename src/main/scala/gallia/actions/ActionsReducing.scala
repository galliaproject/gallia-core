package gallia.actions

import aptus.Anything_

import gallia._
import gallia.atoms.AtomsReducing._
import gallia.heads.reducing._
import gallia.heads.reducing.ReducingPair

// ===========================================================================
@gallia.NumberAbstraction
object ActionsReducing {

  case class Reduce(pairs: Seq[ReducingPair]) extends ActionZUc {
      def vldt  (in: Cls): Errs    = Nil//TODO; at least one pair, no duplicates
      def _meta (in: Cls): Cls    = pairs.map(_.field(in)).thn(Cls.apply)
      def atomzu(in: Cls): AtomZU = pairs.map(_.dataTriplet(in)).thn(_Reduce)
    }

    // ---------------------------------------------------------------------------
    case class Reduce1(targets: TqKeyz, tipe: ReducingType) extends ActionZUc {
      def vldt (in: Cls): Errs = Nil

      def _meta (in: Cls): Cls    = pairs(in).map(_.field(in)).thn(Cls.apply)
      def atomzu(in: Cls): AtomZU = pairs(in).map(_.dataTriplet(in)).thn(_Reduce) //targets.resolve(in).map { key => ReducingDataTriplet1(key, tipe, in.field(key).info.numericalTypeOpt) }.thn(_Reduce)

      private def pairs(in: Cls) = targets.resolve(in).map(ReducingPair1(_, tipe))
    }

    // ---------------------------------------------------------------------------
    case class ReduceN(targets: TqKeyz, tipes: Seq[ReducingType] /* TODO: check at least one*/) extends ActionZUc {
      def vldt  (in: Cls): Errs = Nil
      def _meta (in: Cls): Cls    = pairs(in).map(_.field(in)).thn(Cls.apply)
      def atomzu(in: Cls): AtomZU = pairs(in).map(_.dataTriplet(in)).thn(_Reduce)
        //targets.resolve(in).map { key => ReducingDataTripletN(key, tipes, in.field(key).info.numericalTypeOpt) }.thn(_Reduce)

      private def pairs(in: Cls) = targets.resolve(in).map(ReducingPairN(_, tipes))
    }

}

// ===========================================================================
