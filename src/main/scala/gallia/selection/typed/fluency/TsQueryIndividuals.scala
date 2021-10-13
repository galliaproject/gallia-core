package gallia
package selection.typed.fluency

// ===========================================================================
@TypeMatching // very boilerplatey; TODO: t210124092716 - codegen (very boilerplaty)
object TsQueryIndividuals {

  trait HasOneTypedSel [A, Ignored] { def typed[T: WTT](sel: A => Any) = new One_[T](sel(allSelections[A])) }

      trait HasOneStringSel [A, Ignored] { def string (sel: A => Any) = new One_[String ](sel(allSelections[A])) }
      trait HasOneIntSel    [A, Ignored] { def int    (sel: A => Any) = new One_[Int    ](sel(allSelections[A])) }
      trait HasOneDoubleSel [A, Ignored] { def double (sel: A => Any) = new One_[Double ](sel(allSelections[A])) }
      trait HasOneBooleanSel[A, Ignored] { def boolean(sel: A => Any) = new One_[Boolean](sel(allSelections[A])) }

    // ---------------------------------------------------------------------------
    trait HasOptTypedSel [A, Ignored] { def typed_[T: WTT](sel: A => Any) = new Opt_[T](sel(allSelections[A])) }

      trait HasOptStringSel [A, Ignored] { def string_ (sel: A => Any) = new Opt_[String ](sel(allSelections[A])) }
      trait HasOptIntSel    [A, Ignored] { def int_    (sel: A => Any) = new Opt_[Int    ](sel(allSelections[A])) }
      trait HasOptDoubleSel [A, Ignored] { def double_ (sel: A => Any) = new Opt_[Double ](sel(allSelections[A])) }
      trait HasOptBooleanSel[A, Ignored] { def boolean_(sel: A => Any) = new Opt_[Boolean](sel(allSelections[A])) }

    // ---------------------------------------------------------------------------
    trait HasNesTypedSel [A, Ignored] { def typeds[T: WTT](sel: A => Any) = new Nes_[T](sel(allSelections[A])) }

      trait HasNesStringSel [A, Ignored] { def strings (sel: A => Any) = new Nes_[String ](sel(allSelections[A])) }
      trait HasNesIntSel    [A, Ignored] { def ints    (sel: A => Any) = new Nes_[Int    ](sel(allSelections[A])) }
      trait HasNesDoubleSel [A, Ignored] { def doubles (sel: A => Any) = new Nes_[Double ](sel(allSelections[A])) }
      trait HasNesBooleanSel[A, Ignored] { def booleans(sel: A => Any) = new Nes_[Boolean](sel(allSelections[A])) }

    // ---------------------------------------------------------------------------
    trait HasPesTypedSel [A, Ignored] { def typeds_[T: WTT](sel: A => Any) = new Pes_[T](sel(allSelections[A])) }

      trait HasPesStringSel [A, Ignored] { def strings_ (sel: A => Any) = new Pes_[String ](sel(allSelections[A])) }
      trait HasPesIntSel    [A, Ignored] { def ints_    (sel: A => Any) = new Pes_[Int    ](sel(allSelections[A])) }
      trait HasPesDoubleSel [A, Ignored] { def doubles_ (sel: A => Any) = new Pes_[Double ](sel(allSelections[A])) }
      trait HasPesBooleanSel[A, Ignored] { def booleans_(sel: A => Any) = new Pes_[Boolean](sel(allSelections[A])) }

    // ---------------------------------------------------------------------------
    trait HasXTypedSel[A, Ignored] { def typedx[T: WTT](sel: A => Any) = new One_[T](sel(allSelections[A]), ignoreContainer = true) }

      trait HasXStringSel [A, Ignored] { def stringx (sel: A => Any) = new One_[String ](sel(allSelections[A]), ignoreContainer = true) }
      trait HasXIntSel    [A, Ignored] { def intx    (sel: A => Any) = new One_[Int    ](sel(allSelections[A]), ignoreContainer = true) }
      trait HasXDoubleSel [A, Ignored] { def doublex (sel: A => Any) = new One_[Double ](sel(allSelections[A]), ignoreContainer = true) }
      trait HasXBooleanSel[A, Ignored] { def booleanx(sel: A => Any) = new One_[Boolean](sel(allSelections[A]), ignoreContainer = true) }

    // ===========================================================================
    import selection.untyped.UtsOps._

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
