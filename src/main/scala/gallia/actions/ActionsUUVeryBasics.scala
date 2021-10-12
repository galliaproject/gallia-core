package gallia
package actions

import aptus.{Anything_, String_}

import target._
import vldt.ErrorId
import domain.{KVEs, RVEs}
import atoms.AtomsOthers._
import atoms.AtomsUUVeryBasics._
import actions.utils.RenameDynamicallyHelper
import actions.utils.ActionsUtils._

// ===========================================================================
object ActionsUUVeryBasics {

  case object IdentityUU extends IdentityVM1 with IdentityUUa

  // ---------------------------------------------------------------------------
  class ValidateBObj(value: BObj) extends ActionUUa {
        def  vldt(ignored: Cls): Errs  = _vldt.validateBObj(value)
        def _meta(ignored: Cls): Cls   = value.forceCls
        def atomuus              : AtomUUs = Nil//_IdentityUU.in.seq
      }

    // ---------------------------------------------------------------------------
    class ValidateBObjs(value: BObjs) extends ActionUUa {
        def  vldt(ignored: Cls): Errs  = _vldt.validateBObjs(value)
        def _meta(ignored: Cls): Cls   = value.forceCls
        def atomuus              : AtomUUs = Nil//_IdentityZZ.in.seq
      }

  // ---------------------------------------------------------------------------
  case class ShowSchema(abort: Boolean) extends IdentityV1 with ActionM1 with IdentityUUa { // TODO: t210115180737 - configurable
      def _meta(in: Cls ): Cls = {
        in.printShort()
        if (abort) "210106172924 - aborting".p__ // TODO

        in
      } }

  // ===========================================================================
  case class ReorderKeys(f: Seq[SKey] => Seq[SKey], recursively: Boolean) extends ActionUUd {
      def  vldt(c: Cls): Errs = _vldt.checkKeysReordering(c, f, recursively)
      def _meta(c: Cls): Cls  = c.reorderKeysRecursively(tmp, recursively)
      def atomuu: AtomUU =
        if (recursively) _ReorderKeysRecursively(tmp)
        else             _ReorderKeys           (tmp)

      // ---------------------------------------------------------------------------
      private def tmp: Seq[Key] => Seq[Key] = _.map(_.name).pipe(f).map(_.symbol)
    }

  // ===========================================================================
  case class Rename(target: RPathz) extends ActionUUa {
      def  vldt(c: Cls): Errs    = _vldt.fieldsRenaming(c, target)
      def _meta(c: Cls): Cls     = target.foldLeft(c)(_ rename _)
      def atomuus      : AtomUUs = target.pipe(_atoms(_ => _IdentityUU)) // note: Identities get removed
    }

    // ===========================================================================
    case class RenameDynamically(modifier: SKey => SKey, recursively: Boolean) extends ActionUUb { // TODO: move to somewhat basics
      private def results(c: Cls) = RenameDynamicallyHelper.Results.parse(modifier, recursively)(c)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        results(c).validate(
          (path, throwable) => Err(s"${ErrorId.CouldNotRenameDynamically} - ${path} - ${throwable.getMessage}"),
          qpathz            => _vldt.fieldsRenaming(c, qpathz))

      def _meta  (c: Cls): Cls   =   results(c).forceRPathz.foldLeft(c)(_ rename _)
      def atomuus(c: Cls): AtomUUs = results(c).forceRPathz.pipe(_atoms(_, _ => _IdentityUU))
    }

  // ===========================================================================
  class Remove(target: TqRPathz) extends ActionUUb {
    def  vldt  (c: Cls): Errs    = target.vldtAsOrigin(c) ++ _vldt.someFieldsAreLeft (c, target.size(c)) // see 201107145004

    //FIXME: if nesting?
    def _meta  (c: Cls): Cls     = target.qpathz_(c).forceKPathz.foldLeft(c)(_ remove _)
    def atomuus(c: Cls): AtomUUs = target.qpathz_(c).forceKPathz.pipe(_atoms(_, _Remove))
  }

  // ===========================================================================
  class Retain(target: TqRPathz) extends ActionUUb { import gallia.data.single.RetainMapping

    def  vldt(c: Cls): Errs  =
      target.vldtAsOrigin(c) //TODO: more?
      //target.vldt(c) { _vldt.fieldsRenaming(c, _) }
      //TODO: validate no ambiguous overlap eg both 'p and 'p|>'f

    // ---------------------------------------------------------------------------
    def _meta(c: Cls): Cls =
      target
        .qpathz_(c)
        .pipe { targets =>
          targets
            .fromz.pipe(c.retain)
            .pipe(_.rename(targets)) }

    // ---------------------------------------------------------------------------
    def atomuus(c: Cls): AtomUUs =
        target.qpathz_(c).fromz.pipe { targets => _Retain(targets, RetainMapping(targets.mapping)) } +:
        target.qpathz_(c)      .flatMap(potentialRenaming(_).toSeq)
  }

  // ===========================================================================
  class Add(entries: KVEs) extends ActionUUa {
      def vldt(c: Cls): Errs = Nil ++
        _vldt.fieldsAbsence(c, entries.keys) ++
        _vldt.validateKVEs (   entries) // TODO: t210128155944 - validate new key names

      def _meta(c: Cls): Cls     = entries.forceMetaEntries.foldLeft(c)(_ add _)
      def atomuus      : AtomUUs = entries.forceDataEntries.map(_Add.tupled)
    }

    // ---------------------------------------------------------------------------
    class Replace(entries: RVEs) extends ActionUUa {
      def  vldt(c: Cls): Errs  = Nil ++
        _vldt.fieldsRenaming(c, entries.renz) ++
        _vldt.validateKVEs  (   entries.kves) // TODO: t210128155944 - validate new key names

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = entries.forceMetaEntries.foldLeft(c)(_ replace _)

      def atomuus: AtomUUs =
        entries
          .forceDataEntries
          .flatMap { case (ren, value) =>
            potentialRenaming(ren).toSeq :+
            _Add(ren.to, value) }
    }

}

// ===========================================================================
