package gallia.vldt

import aptus.{String_, Tuple2_}

import gallia._
import gallia.reflect._
import gallia.domain.KVEs
import gallia.target._

// ===========================================================================
object MetaValidationHelper {
  import MetaValidation._

  // ---------------------------------------------------------------------------
  def fieldsRenaming(c: Cls, paths: RPathz): Errs = Nil ++ //TODO: check not empty (likely an error) + check some left
    paths.froms.flatMap(fieldPresence(c, _)) ++
    paths.  tos.flatMap(fieldAbsence (c, _)) ++
    //
    paths.froms.pipe(distinctKPaths) ++
    paths.  tos.pipe(distinctKPaths) ++
    //
    disjointPathss(paths.froms, paths.tos)

  // ===========================================================================
  def isValidTypePair(tipe: TypeNode): (NodeDesc, Boolean) =
    tipe
      .nodeDesc
      .pipe { desc =>
        (desc, NodeDescUtils.isValid(desc)) }

  // ---------------------------------------------------------------------------
  /** if not BObj(s)/cc(s) */
  def validType(location: Location, tipe: TypeNode): Errs =
      isValidTypePair(tipe)
        .pipe { case (desc, valid) =>
          if (valid) Nil
          else       NodeDescUtils.errorMessages(desc).map(_.prepend(location.formatDefault.space) /* TODO: t201114160635 */).map(err) }

  // ===========================================================================
  def typeCompatibility[$Target](c: Cls, duo: Duo[$Target], mode: SpecialCardiMode): Errs =
      if (duo.node.isWhatever0) Nil
      else
          validType(Location.Root, duo.node) ++
          (duo.target match {
              case kpath : KPath  =>                                 _typeCompatibility(c)(kpath, duo, mode)
              case qpathz: RPathz => qpathz.froms.flatMap { kpath => _typeCompatibility(c)(kpath, duo, mode) } })

    // ---------------------------------------------------------------------------
    private def _typeCompatibility(c: Cls)(kpath: KPath, ht: HasTypeNode, mode: SpecialCardiMode): Err_ =
      (   c.field_(kpath).map(_.info),
          ht.infoOpt(isValidType))
        .toOptionalTuple
        .flatMap { case (infoA, infoB) =>
          _infoCompatilibity(kpath, infoA, infoB, mode) }

  // ===========================================================================
  def validateBObjs(value: BObjs): Errs = {
    val tmp = value.values.flatMap(validateBObj)

    tmp ++
      (if (tmp.nonEmpty) Nil // shortcut validation
       else              value.values.map(_.forceCls).pipe(validateCompatibility))
  }

  // ---------------------------------------------------------------------------
  def validateKVEs(location: Location)(entries: KVEs): Errs =
    validateKeys(location)(entries.keyz) ++
    entries
      .values
      .flatMap { entry =>
        location
          .addKey(entry.key)
          .pipe(entry.vldt) }

  // ===========================================================================
  def validateCaseClass(location: Location)(leaf: TypeLeaf): Errs =
    validateKeys(location)(leaf.keyz) ++
    leaf
      .fields
      .flatMap { field =>
        validType(
          field.key.pipe(location.addKey),
          field.node) }

}

// ===========================================================================
