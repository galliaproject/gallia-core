package gallia
package vldt

import aptus.Seq_
import meta._
import reflect._
import target._
import domain._

// ===========================================================================
private[gallia] object MetaValidation {
  private val _helper = MetaValidationHelper

  // ===========================================================================
  // type validity

  private[vldt] def isValidType(tipe: TypeNode): Boolean = isValidTypePair(tipe)._2

  // ---------------------------------------------------------------------------
  def isValidTypePair(tipe: TypeNode): (NodeDesc, Boolean) = _helper.isValidTypePair(tipe)

  // ---------------------------------------------------------------------------
  def validType(                    tipe: TypeNode): Errs = validType(Location.Root, tipe)
  def validType(location: Location, tipe: TypeNode): Errs = _helper.validType(location, tipe)
  def validType(pair: (Location, TypeNode)): Errs = validType(pair._1, pair._2)

  // ---------------------------------------------------------------------------
  def validType (c: Cls, ht :     HasType ): Errs =                   validType(Location.Root, ht.typeNode)
  def validTypes(c: Cls, hts: Seq[HasType]): Errs = hts.flatMap(ht => validType(Location.Root, ht.typeNode))

  // ---------------------------------------------------------------------------
  def classCompatibilities(cls1: Cls, cls2: Cls): Err_ =
      errIf_(!MetaValidationCompatibility.compatible(cls1, cls2))(_Error.SchemaMismatch(cls1, cls2).err)

  // ===========================================================================
  // type compability: check type validity too but not presence

  def typeCompatibility[$Target](c: Cls, duo: Duo[$Target], mode: SpecialCardiMode): Errs = _helper.typeCompatibility(c, duo, mode)

  // ===========================================================================
  // enums

  def checkIsEnumField       (c: Cls)(target: TargetQuery[RPathz])                                     : Errs = _helper._checkField            (c)(target)(_.isEnum)(_Error.NotAnEnumField.apply)
  def checkAreValidEnumValues(c: Cls)(target: TargetQuery[RPathz])(f: Seq[EnumValue] => Seq[EnumValue]): Errs = _helper.checkAreValidEnumValues(c)(target)(f)
  def checkAreValidEnumValues(values: Seq[EnumValue])                                                  : Err_ = _helper.checkAreValidEnumValues(values)

  // ===========================================================================
  // union types
  def checkIsUnionField(c: Cls)(target: Key): Errs = _helper._checkField(c)(TargetQuery.fromKey(target))(_.isUnionType)(_Error.NotAnUnionField.apply)

  // ===========================================================================
  def checkKeysReordering(c: Cls, f: Seq[SKey] => Seq[SKey], recursively: Boolean): Errs = {
    val nesting :Errs =
      if (!recursively) Nil
      else
        c .fields
          .flatMap {
            _ .nestedClassOpt
              .toSeq
              .flatMap(checkKeysReordering(_, f, true)) }

    val origin      =   c.skeys
    val destination = f(c.skeys)

    _Error.InvalidKeyReordering(origin, destination).errsIf(origin.sorted != destination.sorted) ++ nesting
  }

  // ===========================================================================
  def checkNonRequired(c: Cls, targets: KPathz): Errs = targets.flatMap { path => _Error.Tmp(path, " is required"    ).errsIf(c.isRequired(path)) }
  def checkRequired   (c: Cls, targets: KPathz): Errs = targets.flatMap { path => _Error.Tmp(path, " is not required").errsIf(c.isOptional(path)) }
  def checkNonMultiple(c: Cls, targets: KPathz): Errs = targets.flatMap { path => _Error.Tmp(path, " is multiple"    ).errsIf(c.isMultiple(path)) }
  def checkMultiple   (c: Cls, targets: KPathz): Errs = targets.flatMap { path => _Error.Tmp(path, " is not multiple").errsIf(c.isSingle  (path)) }

  // ---------------------------------------------------------------------------
  def checkNotNesting  (c: Cls, target: KPath) = errIf_(!c.field(target).isNesting)      (ErrorId.NotNesting) // eg for TODO
  def checkNotNumerical(c: Cls, target: KPath) = errIf_(!c.field(target).isNumericalType)(ErrorId.NotNumeric) // eg for sum-by

  // ===========================================================================
  def notEmpty(location: Location)(keys: Seq[Key]): Err_ = errIf_(keys.isEmpty)(ErrorId.CantBeEmpty)

  // ---------------------------------------------------------------------------
  def validKeys(location: Location)(keys : Seq[Key]): Errs = keys.flatMap(validKey(location))
  def validKey (location: Location)(key  :     Key ): Err_ = errIf_(!KeyValidation.isValid(key))(s"${ErrorId.InvalidKeyName} - ${location} - ${key}")

  // ===========================================================================
  def distinctRPathz(value: RPathz): Errs = Nil ++
    distinctKPaths(value.fromz) ++
    distinctKPaths(value.toz  ) ++
    disjoint(value.fromz, value.toz)

  def distinctKPaths(path1: KPath, path2: KPath, more: KPath*): Err_ = distinctKPaths(Seq(path1, path2) ++ more)

  def distinctKPaths(paths: KPathz    ): Err_ = distinctKPaths(paths.values)
  def distinctKPaths(paths: Seq[KPath]): Err_ = errIf_(!paths.isDistinct)("distinctpaths", paths.duplicates)

  // ---------------------------------------------------------------------------
  def distinctKeys(location: Location)(keys: Seq[Key]) = errIf_(!keys.isDistinct)(s"${ErrorId.DuplicateKeys} -- ", keys.duplicates)

  def disjointPathss(paths1: Seq[KPath], paths2: Seq[KPath]) = errIf_(!paths1.isDisjointWith(paths2))(s"${ErrorId.NotDisjoint} ${paths1} ${paths2}")
  def disjointKeyss (keys1: Seq[Key], keys2: Seq[Key])       = errIf_(!keys1 .isDisjointWith(keys2 ))(s"${ErrorId.NotDisjoint} ${keys1} ${keys2}")

  def disjoint(values1: KPathz, values2: KPathz)              = errIf_(!values1.isDisjointWith(values2))(s"${ErrorId.NotDisjoint} ${values1} ${values1}")

  // ===========================================================================
  def someFieldsAreLeft(c: Cls, size: Int) = errIf_(c.size <= size)(s"${ErrorId.NoFieldsLeft}")

  // ===========================================================================
  def fieldRenamings(c: Cls, keys: Seq[Ren]): Errs = keys.foldLeft(Seq[Err]()) { _ ++ fieldRenaming(c, _) } //TODO: disjointness

  // ---------------------------------------------------------------------------
  def fieldsRenaming(c: Cls, keys : Renz  ): Errs = _helper.fieldsRenaming(c, keys.rpathz)
  def fieldsRenaming(c: Cls, paths: RPathz): Errs = _helper.fieldsRenaming(c, paths)

  // ---------------------------------------------------------------------------
  def fieldRenaming(c: Cls, key: Ren): Errs = Nil ++
                      fieldPresence(c, key.from) ++
    key.toOpt.flatMap(fieldAbsence (c, _))

  def fieldRenaming(c: Cls, path: RPath): Errs = Nil ++
                       fieldPresence(c, path.from) ++
    path.pathOpt.flatMap(fieldAbsence (c, _))

  // ---------------------------------------------------------------------------
  def fieldsPresence(c: Cls, paths: KPathz): Errs =
    paths.values.flatMap(fieldPresence(c, _)) ++
    paths.values.pipe    (distinctKPaths)

  // ---------------------------------------------------------------------------
  def fieldPresence(c: Cls, path: KPath) = errIf_(!c.contains(path)    )(s"${ErrorId.NoSuchField}", path, c)
  def fieldPresence(c: Cls, key: Key   ) = errIf_(!c.keys.contains(key))(s"${ErrorId.NoSuchField}", key, c)

  // ---------------------------------------------------------------------------
  def fieldAbsence(c: Cls, path: KPath) = errIf_(c.contains(path)    )(s"${ErrorId.FieldAlreadyExists}", path, c)
  def fieldAbsence(c: Cls, key : Key  ) = errIf_(c.keys.contains(key))(s"${ErrorId.FieldAlreadyExists}", key, c) // TODO: pass actul existing field

  // ---------------------------------------------------------------------------
  def fieldsPresence(c: Cls, keys: Seq[Key]): Errs = keys       .flatMap(fieldPresence(c, _))
  def fieldsPresence(c: Cls, keys: Keyz    ): Errs = keys.values.flatMap(fieldPresence(c, _))

  // ---------------------------------------------------------------------------
  def fieldsAbsence(c: Cls, keys: Seq[Key]): Errs = keys       .flatMap(fieldAbsence(c, _))
  def fieldsAbsence(c: Cls, keys: Keyz    ): Errs = keys.values.flatMap(fieldAbsence(c, _))
  def fieldsAbsence(c: Cls, keys: KPathz  ): Errs = keys.values.flatMap(fieldAbsence(c, _))

  // ===========================================================================
  def validateBObj                    (value: BObj ): Errs = validateKVEs(Location.Root)(value.entries)
  def validateBObj(location: Location)(value: BObj ): Errs = validateKVEs(location     )(value.entries)

  def validateBObjs                   (value: BObjs): Errs = _helper.validateBObjs(value)

  // ---------------------------------------------------------------------------
  def validateKVEs                    (entries: KVEs): Errs = validateKVEs(Location.Root)(entries)
  def validateKVEs(location: Location)(entries: KVEs): Errs = _helper.validateKVEs(location)(entries)

  // ===========================================================================
  // eg if field f is int in one and double in another - see Cls.union for starting point
  def validateCompatibility(values: Seq[Cls]): Errs = Nil // FIXME
  def validateCompatibility(c1: Cls, c2: Cls): Errs = Nil // FIXME

  def validateCaseClass(location: Location)(leaf: TypeLeaf): Errs = _helper.validateCaseClass(location)(leaf)

  // ===========================================================================
  private[vldt] def validateKeys(location: Location)(keys: Keyz): Errs = Nil ++
    this.notEmpty    (location)(keys.values) ++
    this.validKeys   (location)(keys.values) ++
    this.distinctKeys(location)(keys.values)

}

// ===========================================================================
