package gallia

import aptus.Anything_

// ===========================================================================
object FunctionWrappers {
  type _ff11 = Any            =>  Any
  type _ff12 = Any            => (Any, Any)
  type _ff13 = Any            => (Any, Any, Any)

  type _ff21 = (Any, Any) =>  Any
  type _ff22 = (Any, Any) => (Any, Any)
  type _ff23 = (Any, Any) => (Any, Any, Any)

  type _ff31 = (Any, Any, Any) =>  Any
  type _ff32 = (Any, Any, Any) => (Any, Any)
  type _ff33 = (Any, Any, Any) => (Any, Any, Any)

  type _ff41 = (Any, Any, Any, Any     ) =>  Any
  type _ff51 = (Any, Any, Any, Any, Any) =>  Any

  type _ff14 = Any => (Any, Any, Any, Any     )
  type _ff15 = Any => (Any, Any, Any, Any, Any)

  // ---------------------------------------------------------------------------
  type _pp11 =  Any                      => Boolean
  type _pp21 = (Any, Any               ) => Boolean
  type _pp31 = (Any, Any, Any          ) => Boolean
  type _pp41 = (Any, Any, Any, Any     ) => Boolean
  type _pp51 = (Any, Any, Any, Any, Any) => Boolean

  // ---------------------------------------------------------------------------
  private type AnyValue1 =  AnyValue
  private type AnyValue2 = (AnyValue, AnyValue)
  private type AnyValue3 = (AnyValue, AnyValue, AnyValue)
  private type AnyValue4 = (AnyValue, AnyValue, AnyValue, AnyValue)
  private type AnyValue5 = (AnyValue, AnyValue, AnyValue, AnyValue, AnyValue)

  type _agg1 = Seq[AnyValue1] => AnyValue1
  type _agg2 = Seq[AnyValue2] => AnyValue1
  type _agg3 = Seq[AnyValue3] => AnyValue1
  type _agg4 = Seq[AnyValue4] => AnyValue1
  type _agg5 = Seq[AnyValue5] => AnyValue1

  // ===========================================================================
  @inline private[gallia] def wrap  [O1](f: O1 => Any): _ff11 = f.asInstanceOf[_ff11]
  @inline private[gallia] def wrap11[O1](f: O1 => Any): _ff11 = f.asInstanceOf[_ff11]

    // ---------------------------------------------------------------------------
    @inline private[gallia] def wrap21[O1, O2            ](f: (O1, O2            ) => Any): _ff21 = f.asInstanceOf[_ff21]
    @inline private[gallia] def wrap31[O1, O2, O3        ](f: (O1, O2, O3        ) => Any): _ff31 = f.asInstanceOf[_ff31]
    @inline private[gallia] def wrap41[O1, O2, O3, O4    ](f: (O1, O2, O3, O4    ) => Any): _ff41 = f.asInstanceOf[_ff41]
    @inline private[gallia] def wrap51[O1, O2, O3, O4, O5](f: (O1, O2, O3, O4, O5) => Any): _ff51 = f.asInstanceOf[_ff51]

    // ---------------------------------------------------------------------------
    @inline private[gallia] def wrap12[O1](f: O1 => (Any, Any               )): _ff12 = f.asInstanceOf[_ff12]
    @inline private[gallia] def wrap13[O1](f: O1 => (Any, Any, Any          )): _ff13 = f.asInstanceOf[_ff13]
    @inline private[gallia] def wrap14[O1](f: O1 => (Any, Any, Any, Any     )): _ff14 = f.asInstanceOf[_ff14]
    @inline private[gallia] def wrap15[O1](f: O1 => (Any, Any, Any, Any, Any)): _ff15 = f.asInstanceOf[_ff15]

    // ---------------------------------------------------------------------------
    @inline private[gallia] def wrap22[O1, O2]    (f: (O1, O2)     => (Any, Any))     : _ff22 = f.asInstanceOf[_ff22]

    @inline private[gallia] def wrap23[O1, O2]    (f: (O1, O2)     => (Any, Any, Any)): _ff23 = f.asInstanceOf[_ff23]
    @inline private[gallia] def wrap32[O1, O2, O3](f: (O1, O2, O3) => (Any, Any))     : _ff32 = f.asInstanceOf[_ff32]

    @inline private[gallia] def wrap33[O1, O2, O3](f: (O1, O2, O3) => (Any, Any, Any)): _ff33 = f.asInstanceOf[_ff33]

  // ===========================================================================
  @inline private[gallia]   def pwrap  [O1                ](f:  O1                  => Boolean): _pp11 = f.asInstanceOf[_pp11]

    @inline private[gallia] def pwrap11[O1                ](f:  O1                  => Boolean): _pp11 = f.asInstanceOf[_pp11]
    @inline private[gallia] def pwrap21[O1, O2            ](f: (O1, O2            ) => Boolean): _pp21 = f.asInstanceOf[_pp21]
    @inline private[gallia] def pwrap31[O1, O2, O3        ](f: (O1, O2, O3        ) => Boolean): _pp31 = f.asInstanceOf[_pp31]
    @inline private[gallia] def pwrap41[O1, O2, O3, O4    ](f: (O1, O2, O3, O4    ) => Boolean): _pp41 = f.asInstanceOf[_pp41]
    @inline private[gallia] def pwrap51[O1, O2, O3, O4, O5](f: (O1, O2, O3, O4, O5) => Boolean): _pp51 = f.asInstanceOf[_pp51]

  // ===========================================================================
  @inline private[gallia]def awrap11[O1                ](f: Seq[ O1                 ] => Any): _agg1 = f.asInstanceOf[_agg1]
  @inline private[gallia]def awrap21[O1, O2            ](f: Seq[(O1, O2            )] => Any): _agg2 = f.asInstanceOf[_agg2]
  @inline private[gallia]def awrap31[O1, O2, O3        ](f: Seq[(O1, O2, O3        )] => Any): _agg3 = f.asInstanceOf[_agg3]
  @inline private[gallia]def awrap41[O1, O2, O3, O4    ](f: Seq[(O1, O2, O3, O4    )] => Any): _agg4 = f.asInstanceOf[_agg4]
  @inline private[gallia]def awrap51[O1, O2, O3, O4, O5](f: Seq[(O1, O2, O3, O4, O5)] => Any): _agg5 = f.asInstanceOf[_agg5]

  // ===========================================================================
  @inline private[gallia]def wrapUU[O1](f: O1 => Any): HeadU => HeadU = f.asInstanceOf[HeadU => HeadU]
  @inline private[gallia]def wrapUZ[O1](f: O1 => Any): HeadU => HeadZ = f.asInstanceOf[HeadU => HeadZ]
  @inline private[gallia]def wrapZZ[O1](f: O1 => Any): HeadZ => HeadZ = f.asInstanceOf[HeadZ => HeadZ]
  @inline private[gallia]def wrapZU[O1](f: O1 => Any): HeadZ => HeadU = f.asInstanceOf[HeadZ => HeadU]

  // ===========================================================================
  import gallia.heads.common._

    @inline private[gallia]def wwrap11(f: _ff11): _ff11 =  v1          => f(new WV(v1))                        .thn(unwrapWhatever)
    @inline private[gallia]def wwrap21(f: _ff21): _ff21 = (v1, v2)     => f(new WV(v1), new WV(v2))            .thn(unwrapWhatever)
    @inline private[gallia]def wwrap31(f: _ff31): _ff31 = (v1, v2, v3) => f(new WV(v1), new WV(v2), new WV(v3)).thn(unwrapWhatever)

@inline private[gallia]def wwwrap21a(f: (WV, WV) => Any): _ff21 = (v1, v2)     => f(new WV(v1), new WV(v2))            .thn(unwrapWhatever)
@inline private[gallia]def wwwrap21b(f: (WV, WV) => Any): _ff21 = (v1, v2)     => f(new WV(v1), new WV(v2))            .thn(unwrapWhatever2)

    // ---------------------------------------------------------------------------
    @inline private[gallia]def __wwrap11a(f: WV => WV): _ff11 = x => f(new Whatever(x)).thn(unwrapWhatever)
    @inline private[gallia]def __wwrap11b(f: WV => Seq[WV]): _ff11 = x => f(new Whatever(x)).thn(unwrapWhatever)

    @inline private[gallia]def __wwrap22a[D](f: WV =>     WV2[D] ): _ff11 = x => f(new Whatever(x)).value
    @inline private[gallia]def __wwrap22b[D](f: WV => Seq[WV2[D]]): _ff11 = x => f(new Whatever(x)).map(_.forceOne /* checked at 210112155301 */)

@inline private[gallia] def __wwrap21[O1, O2](f: (O1, O2) => Any): _ff21 = (v1, v2) => f(v1.asInstanceOf[O1], v2.asInstanceOf[O2]).thn(_unwrapWhatever1)

    // ---------------------------------------------------------------------------
    /*private */def unwrapWhatever(value: Any): Any = value match {
            case seq: Seq [_] => seq.map(_unwrapWhatever1)
            case Some(sgl)    => _unwrapWhatever1(sgl) // TODO: needed?
            case sgl          => _unwrapWhatever1(sgl) }

      /*private */def unwrapWhatever2(value: Any): Any =
         value match {
            case seq: Seq [_] => seq.map(_unwrapWhatever2)
            case Some(sgl)    => _unwrapWhatever2(sgl) // TODO: needed?
            case sgl          => _unwrapWhatever2(sgl) }

      // ---------------------------------------------------------------------------
      private def _unwrapWhatever1(value: Any): Any = value.asInstanceOf[Whatever].any
      private def _unwrapWhatever2(value: Any): Any = value.asInstanceOf[TypedWhatever[_]].value

}

// ===========================================================================
