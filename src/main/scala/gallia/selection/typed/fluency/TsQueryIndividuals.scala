package gallia
package selection.typed.fluency

// ===========================================================================
@TypeMatching // very boilerplatey; TODO: t210124092716 - codegen (very boilerplaty)
object TsQueryIndividuals {

  trait HasOneTypedSel [A, _] { def typed[T: WTT](sel: A => Any) = new One_[T](sel(allSelections[A])) }

      trait HasOneStringSel [A, _] { def string (sel: A => Any) = new One_[String ](sel(allSelections[A])) }
      trait HasOneIntSel    [A, _] { def int    (sel: A => Any) = new One_[Int    ](sel(allSelections[A])) }
      trait HasOneDoubleSel [A, _] { def double (sel: A => Any) = new One_[Double ](sel(allSelections[A])) }
      trait HasOneBooleanSel[A, _] { def boolean(sel: A => Any) = new One_[Boolean](sel(allSelections[A])) }

    // ---------------------------------------------------------------------------
    trait HasOptTypedSel [A, _] { def typed_[T: WTT](sel: A => Any) = new Opt_[T](sel(allSelections[A])) }

      trait HasOptStringSel [A, _] { def string_ (sel: A => Any) = new Opt_[String ](sel(allSelections[A])) }
      trait HasOptIntSel    [A, _] { def int_    (sel: A => Any) = new Opt_[Int    ](sel(allSelections[A])) }
      trait HasOptDoubleSel [A, _] { def double_ (sel: A => Any) = new Opt_[Double ](sel(allSelections[A])) }
      trait HasOptBooleanSel[A, _] { def boolean_(sel: A => Any) = new Opt_[Boolean](sel(allSelections[A])) }

    // ---------------------------------------------------------------------------
    trait HasNesTypedSel [A, _] { def typeds[T: WTT](sel: A => Any) = new Nes_[T](sel(allSelections[A])) }

      trait HasNesStringSel [A, _] { def strings (sel: A => Any) = new Nes_[String ](sel(allSelections[A])) }
      trait HasNesIntSel    [A, _] { def ints    (sel: A => Any) = new Nes_[Int    ](sel(allSelections[A])) }
      trait HasNesDoubleSel [A, _] { def doubles (sel: A => Any) = new Nes_[Double ](sel(allSelections[A])) }
      trait HasNesBooleanSel[A, _] { def booleans(sel: A => Any) = new Nes_[Boolean](sel(allSelections[A])) }

    // ---------------------------------------------------------------------------
    trait HasPesTypedSel [A, _] { def typeds_[T: WTT](sel: A => Any) = new Pes_[T](sel(allSelections[A])) }

      trait HasPesStringSel [A, _] { def strings_ (sel: A => Any) = new Pes_[String ](sel(allSelections[A])) }
      trait HasPesIntSel    [A, _] { def ints_    (sel: A => Any) = new Pes_[Int    ](sel(allSelections[A])) }
      trait HasPesDoubleSel [A, _] { def doubles_ (sel: A => Any) = new Pes_[Double ](sel(allSelections[A])) }
      trait HasPesBooleanSel[A, _] { def booleans_(sel: A => Any) = new Pes_[Boolean](sel(allSelections[A])) }

    // ---------------------------------------------------------------------------
    trait HasXTypedSel[A, _] { def typedx[T: WTT](sel: A => Any) = new One_[T](sel(allSelections[A]), ignoreContainer = true) }

      trait HasXStringSel [A, _] { def stringx (sel: A => Any) = new One_[String ](sel(allSelections[A]), ignoreContainer = true) }
      trait HasXIntSel    [A, _] { def intx    (sel: A => Any) = new One_[Int    ](sel(allSelections[A]), ignoreContainer = true) }
      trait HasXDoubleSel [A, _] { def doublex (sel: A => Any) = new One_[Double ](sel(allSelections[A]), ignoreContainer = true) }
      trait HasXBooleanSel[A, _] { def booleanx(sel: A => Any) = new One_[Boolean](sel(allSelections[A]), ignoreContainer = true) }

    // ===========================================================================
    import gallia.selection.untyped.UtsOps._

    // ---------------------------------------------------------------------------
    private object _AllSelections //TODO: list them all (codegen:x)
          extends
                  ForKey
            with  ForPath
            with  ForEachKey
            with  ForEachPath

            with  CommonTyped
            with  Rename
            with  Remove
            with  Retain

            with  SetDefault
            with  Translate
            with  RemoveIf

            with  Transform

      // ---------------------------------------------------------------------------
      private[typed] def allSelections[A] = _AllSelections.asInstanceOf[A]

}

// ===========================================================================
