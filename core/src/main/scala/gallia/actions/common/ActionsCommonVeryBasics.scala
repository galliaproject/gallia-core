package gallia
package actions
package common

import aptus.{Anything_, String_}

import trgt._
import vldt.ErrorId
import domain.{KVEs, RVEs}
import atoms.AtomsOthers._
import atoms.common.AtomsCommonVeryBasics._
import actions.utils.RenameDynamicallyHelper
import actions.utils.ActionsUtils._

// ===========================================================================
object ActionsCommonVeryBasics {

  case object IdentityUU extends IdentityVM1 with IdentityUU0N

  // ---------------------------------------------------------------------------
  class ValidateBObj(value: BObj) extends ActionUU0N {
        def  vldt(ignored: Cls): Errs    = _vldt.validateBObj(value)
        def _meta(ignored: Cls): Cls     = value.forceCls
        def atomuus            : AtomUUs = Nil }

    // ---------------------------------------------------------------------------
    class ValidateBObjs(value: BObjs) extends ActionUU0N {
        def  vldt(ignored: Cls): Errs    = _vldt.validateBObjs(value)
        def _meta(ignored: Cls): Cls     = value.forceCls
        def atomuus            : AtomUUs = Nil }

  // ---------------------------------------------------------------------------
  case class ShowSchema(abort: Boolean) extends IdentityV1 with ActionM1 with IdentityUU0N { // TODO: t210115180737 - configurable
      def _meta(in: Cls ): Cls = {
        in.printShort()
        if (abort) "210106172924 - aborting".p__ // TODO

        in } }

  // ===========================================================================
  case class ReorderKeys(f: Seq[SKey] => Seq[SKey], recursively: Boolean) extends ActionUU11 {
        def  vldt (c: Cls): Errs   = _vldt.checkKeysReordering(c, f, recursively)
        def _meta (c: Cls): Cls    = c.reorderKeys(tmp, recursively)
        def atomuu(c: Cls): AtomUU =
          if (recursively) _ReorderKeysRecursively(tmp)
          else             _ReorderKeys           (tmp)
  
        // ---------------------------------------------------------------------------
        private def tmp: Seq[Key] => Seq[Key] = _.map(_.name).pipe(f).map(_.symbol) }
  
    // ---------------------------------------------------------------------------
    case class ReorderSelectedKeys(target: TqKeyz, f: (Seq[SKey], Seq[SKey]) => Seq[SKey]) extends ActionUU11 {
        def  vldt(c: Cls): Errs = 
          target.vldtTargetQuery(c)
            .in.someIf(_.nonEmpty)
            .getOrElse {       
              _vldt.checkKeysReordering(c, f(_, target.resolve(c).skeys), recursively = false) }
  
        def _meta (c: Cls): Cls    = c.reorderKeys(tmp(c), recursively = false)
        def atomuu(c: Cls): AtomUU = _ReorderKeys (tmp(c))

        // ---------------------------------------------------------------------------
        private def tmp(c: Cls): Seq[Key] => Seq[Key] = _.map(_.name).pipe(f(_, target.resolve(c).skeys)).map(_.symbol)
      }

  // ===========================================================================
  case class Rename(target: RPathz) extends ActionUU1Noswo {
      def  vldt  (c: Cls): Errs    = _vldt.fieldsRenaming(c, target)
      def _meta  (c: Cls): Cls     = target.scanLeft(c)(_ rename _).pipe(storeIntraMetas)
      def atomuus(c: Cls): AtomUUs = target.pipe(_atoms(_ => _IdentityUU))
.filterNot(_.isIdentity)
.pipe(updateAtomMetas) }

    // ===========================================================================
    case class RenameDynamically(modifier: SKey => SKey, recursively: Boolean) extends ActionUU1N { // TODO: move to somewhat basics
      private def results(c: Cls) = RenameDynamicallyHelper.Results.parse(modifier, recursively)(c)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        results(c).validate(
          (path, throwable) => Err(s"${ErrorId.CouldNotRenameDynamically} - ${path} - ${throwable.getMessage}"),
          rpathz            => _vldt.fieldsRenaming(c, rpathz))

      def _meta  (c: Cls): Cls     = results(c).forceRPathz.foldLeft(c)(_ rename _)
      def atomuus(c: Cls): AtomUUs = results(c).forceRPathz.pipe(_atoms(_, _ => _IdentityUU))
    }

  // ===========================================================================
  class Remove(target: TqKPathz) extends ActionUU1Noswo {
    def  vldt  (c: Cls): Errs    = target.vldtAsOrigin(c) ++ _vldt.someFieldsAreLeft (c, target.size(c)) // see 201107145004

    //FIXME: t220414112604 - if nesting, ensure parent is removed instead of all sub-fields being removed
    def _meta  (c: Cls): Cls     = target.kpathz_(c).scanLeft(c)(_ remove _) 
.pipe(storeIntraMetas)
    def atomuus(c: Cls): AtomUUs = target.kpathz_(c).pipe(_atoms(_, _Remove))
.pipe(updateAtomMetas) }

  // ===========================================================================
  class Retain(target: TqRPathz) extends ActionUU1Noswo { import gallia.data.single.RetainMapping

    def  vldt(c: Cls): Errs  =
      target.vldtAsOrigin(c) //TODO: more?
      //target.vldt(c) { _vldt.fieldsRenaming(c, _) }
      //TODO: validate no ambiguous overlap eg both 'p and 'p|>'f

    // ---------------------------------------------------------------------------
    def _meta(c: Cls): Cls =
      target
        .rpathz_(c)
        .pipe { targets =>
          targets
            .fromz.pipe(c.retain)
            .rename(targets) }

    // ---------------------------------------------------------------------------
    def atomuus(c: Cls): AtomUUs =
      target.rpathz_(c).fromz.pipe { targets => _Retain(RetainMapping(targets.mapping)) } +:
      target.rpathz_(c)      .flatMap(potentialRenaming(_).toSeq)
  }

  // ===========================================================================
  class Add(entries: KVEs) extends ActionUU1Noswo {
      def vldt(c: Cls): Errs = Nil ++
        _vldt.fieldsAbsence(c, entries.keys) ++
        _vldt.validateKVEs (   entries) // TODO: t210128155944 - validate new key names

      def _meta  (c: Cls): Cls     = entries.forceMetaEntries.scanLeft(c)(_ add _)      
.pipe(storeIntraMetas)
      def atomuus(c: Cls): AtomUUs = entries.forceDataEntries.map((_Add.apply _).tupled)
.pipe(updateAtomMetas) }

    // ---------------------------------------------------------------------------
    class Replace(entries: RVEs) extends ActionUU1Noswo {
      def  vldt(c: Cls): Errs  = Nil ++
        _vldt.fieldsRenaming(c, entries.renz) ++
        _vldt.validateKVEs  (   entries.kves) // TODO: t210128155944 - validate new key names

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = entries.forceMetaEntries.scanLeft(c)(_ replace _)
.pipe(storeIntraMetas)

      def atomuus(c: Cls): AtomUUs =
        entries
          .forceDataEntries
          .flatMap { case (ren, value) =>
            potentialRenaming(ren).toSeq :+
            _Add(ren.to, value) }
.pipe(updateAtomMetas) //FIXME: if renames
    }

}

// ===========================================================================
