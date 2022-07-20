package gallia
package atoms

import aptus.Anything_
import aptus.Separator

// ===========================================================================
object AtomsOthers {

  case class _Nested(parent: KPath, nestee: AtomUU) extends AtomUU { def naive(o: Obj) = {
      // TODO: opt: meta-level info (common combinations)
      val f = (x: Any) =>
        x match {
          case y: Seq[_] => y.map(_.asInstanceOf[Obj]).map(nestee.naive)
          case y         => y      .asInstanceOf[Obj] .pipe(nestee.naive) }

      o.transformPath(parent, f)
    }
  }

  // ===========================================================================
  case class _Unpivot(keyz: Keyz) extends AtomUU { def naive(o: Obj) =
    o.unpivot(keyz) }

  // ===========================================================================
  case object _IdentityUU extends AtomUU { @inline def naive(o: Obj ) = o }
  case object _IdentityZZ extends AtomZZ { @inline def naive(z: Objs) = z }

  // ===========================================================================
  case class _CheckpointU(c: Cls, path1: String, path2: String) extends AtomUU { def naive(o: Obj) = {
      gallia.atoms.AtomsXO._SchemaOutputU (c                          , path1, io.UrlLike.Plain, suffix = "").naive(o)    
      gallia.atoms.AtomsXO._UrlLikeOutputU(io.IoTypeU.PrettyJsonObject, path2, io.UrlLike.Plain)             .naive(o)
  
      o } }
    
    // ---------------------------------------------------------------------------
    case class _CheckpointZ(c: Cls, path1: String, path2: String) extends AtomZZ { def naive(z: Objs) = {
      gallia.atoms.AtomsXO._SchemaOutputZ (c                   , path1, io.UrlLike.Plain, suffix = "").naive(z)    
      gallia.atoms.AtomsXO._UrlLikeOutputZ(io.IoTypeZ.JsonLines, path2, io.UrlLike.Gzipped)           .naive(z)
  
      assert(!z.isIteratorBased) // FIXME: t220209132326
      z } }

  // ===========================================================================
  case class _MapV2V   (f: _ff11) extends AtomVV   { def naive(v : Vle)         : Vle = f(v) }
  case class _CombineVV(f: _ff21) extends AtomVv2V { def naive(v1: Vle, v2: Vle): Vle = f(v1, v2) }

  // ===========================================================================
  case class _InspectU(msg: Option[String], abort: Boolean) extends AtomUU { def naive(u: Obj) =
      { msg.foreach(println); u.pp.tapIf(_ => abort) { x => x.__exit; () } } }

    // ---------------------------------------------------------------------------
    @Distributivity
    case class _InspectZ(msg: Option[String], abort: Boolean) extends AtomZZ { def naive(z: Objs) =
      { msg.foreach(println); z.pp.tapIf(_ => abort) { x => x.__exit; () } } }

  // ===========================================================================
  case object _Merge extends AtomUUtoU { def naive(o1: Obj, o2: Obj): Obj =
      o1.merge(o2) }

  // ===========================================================================
  case class _SquashUUnsafe(f:     Obj  => AnyValue) extends AtomUV { def naive(o: Obj ) = f(o) }
  case class _SquashZUnsafe(f: Seq[Obj] => AnyValue) extends AtomZV { def naive(o: Objs) = f(o.toListAndTrash) }

  // ===========================================================================
  // zu

  case object _ForceOne extends AtomZU { def naive(z: Objs) = {    
    val itr = z.closeabledIterator
    if (!itr.hasNext) { itr.close(); _Error.Runtime.EmptyStream.throwDataError() }
    else {
      val next = itr.next()
      if (itr.hasNext) { itr.close(); _Error.Runtime.MoreThanNElements(n = 1).throwDataError() }
      else             { itr.close(); next } } } }

  // ---------------------------------------------------------------------------
  case class _AsArray1(key: Key) extends AtomZU { def naive(z: Objs) =
    obj(key -> z.toListAndTrash.map(_.forceKey(key))) }

  // ---------------------------------------------------------------------------
  case class _AsArray2(newKey: Key) extends AtomZU { def naive(z: Objs) =
    obj(newKey -> z.toListAndTrash) }

  // ---------------------------------------------------------------------------
  case object _MergeAll extends AtomZU { def naive(z: Objs) =
    z.toListAndTrash.reduceLeft(_ merge _) } //z.toList.map(_.data).reduceLeft(_ ++ _).pipe(obj)

  // ===========================================================================
  @Scalability case class _Pivone(keyKey: Key, valueKey: Key) extends AtomZU { def naive(z: Objs) =
      z .toListAndTrash
        .flatMap { o =>
          val newKey =
            o .unarrayCompositeKey2(keyKey)
              .getOrElse(dataError(ErrorId.Runtime.EmptyKey))
  
          o.attemptKey(valueKey).map(newKey -> _) }
       .pipe(obj) }
       // t201122154119 - if in is empty...
      
    // ---------------------------------------------------------------------------
    @Scalability @deprecated case class _UnarrayEntries0(keyKeys: Keyz, separator: Separator, valueKey: Key) extends AtomZU { def naive(z: Objs) =
        z .toListAndTrash
          .flatMap { o =>
            val newKey =
              o .unarrayCompositeKey(keyKeys.values, separator)
                .getOrElse(dataError(ErrorId.Runtime.NoKeysLeft)) //TODO: or offer alterative if all missing?
    
            o.attemptKey(valueKey).map(newKey -> _) }
         .pipe(obj) }
         // t201122154119 - if in is empty...
    
    // ---------------------------------------------------------------------------
    @Scalability @deprecated case class _UnarrayBy0(keys: Keyz, sep: Separator) extends AtomZU { def naive(z: Objs) =
        // FIXME: runtime check of keys
        z .toListAndTrash
          .flatMap { o =>
            val newKey =
              o .unarrayCompositeKey(keys.values, sep)
                .getOrElse(dataError(ErrorId.Runtime.NoKeysLeft)) //TODO: or offer alterative if all missing?
  
            o.removeOpt(keys).map(newKey -> _) }
          .pipe(obj) }

  // ===========================================================================
  // uz

  case object _ConvertUtoZ extends AtomUZ { def naive(u: Obj) = Objs.splat(u) }

  // ---------------------------------------------------------------------------
  case class _FlattenByU(target: KPath) extends AtomUZ { def naive(u: Obj) =
      _FlattenBy(target)(u).pipe(Objs.from) }

    // ---------------------------------------------------------------------------
    // actually zz
    case class _FlattenByZ(target: KPath) extends AtomZZ { def naive(z: Objs) =
      z.flatMap(_FlattenBy(target)) }

    // ---------------------------------------------------------------------------
    private object _FlattenBy {
      def apply(target: KPath)(u: Obj): List[Obj] =
        u .attemptPath(target)
          .map(_.asInstanceOf[List[_]])
          .map {
            _ .map(u.putPath(target, _))
              .toList }
          .getOrElse(List(u))
    }
}

// ===========================================================================
