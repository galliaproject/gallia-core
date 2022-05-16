package gallia
package vldt

import aptus.{String_, Tuple2_}

import reflect._
import target._
import domain.KVEs

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
              case key   : Key    =>                                 _typeCompatibility(c)(KPath.from(key),      duo, mode)
              case ren   : Ren    =>                                 _typeCompatibility(c)(KPath.from(ren.from), duo, mode)
              case kpath : KPath  =>                                 _typeCompatibility(c)(kpath,                duo, mode)
              case qpathz: RPathz => qpathz.froms.flatMap { kpath => _typeCompatibility(c)(kpath,                duo, mode) } })

    // ---------------------------------------------------------------------------
    private def _typeCompatibility(c: Cls)(kpath: KPath, ht: HasTypeNode, mode: SpecialCardiMode): Err_ =
      (   c.field_(kpath).map(_.ofni), // see t210125111338 (union types)
          ht.ofnuOpt(isValidType))
        .toOptionalTuple
        .flatMap { case (ofniA, ofniB) =>
          errIf_(!MetaValidationCompatibility.compatible(ofniA, ofniB, mode)) {
            _Error.TypeMismatch(kpath, ofniA, ofniB, mode).err } }

  // ===========================================================================
  def _checkField(c: Cls)(target: TargetQuery[RPathz])(pred: Fld => Boolean)(f: KPath => _Error3): Errs =
    target
      .resolve(c)
      .map(_.from)
      .flatMap { kpath =>
        if (pred(c.field(kpath))) None
        else                       Some(f(kpath).err) }

  // ===========================================================================
  def checkAreValidEnumValues(c: Cls)(target: TargetQuery[RPathz])(f: Seq[EnumValue] => Seq[EnumValue]): Errs =
    target
      .resolve(c)
      .map(_.from)
      .flatMap { path =>
        c.field(path).containees.flatMap {
          case BasicType._Enm(origin) => checkAreValidEnumValues(f(origin))
          case _                      => None } }

  // ---------------------------------------------------------------------------
  def checkAreValidEnumValues(values: Seq[EnumValue]): Err_ =
      if (values.isEmpty || values.distinct != values) Some(_Error.InvalidEnumStringValues(values.map(_.stringValue)).err)
      else                                             None

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
