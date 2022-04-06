package gallia
package meta

import aptus.Seq_

// ===========================================================================
trait ClsAdvanced { self: Cls =>

  def containsMatching(pred: Fld => Boolean): Boolean =
    fields.exists { field =>
      field
        .nestedClassOpt
        .map(_.containsMatching(pred))
        .getOrElse(pred(field)) }

  // ---------------------------------------------------------------------------
  def basicTypeSet: Set[BasicType] =
    fields
      .flatMap { field =>
        field
          .nestedClassOpt
          .map      (_.basicTypeSet)
          .getOrElse(Set(field.forceBasicType)) }  
      .toSet

  // ===========================================================================
  /** expects no conflicts                   */ def mergeDisjoint  (that: Cls): Cls = that.fields.foldLeft(this)(_ add _)
  /** expects common fields to be compatible */ def unionCompatible(that: Cls): Cls =
    Cls(
      fields
        .map { thisField =>
          that.field_(thisField.key) match {
            case None            => thisField.toNonRequired
            case Some(thatField) => Fld(thisField.key, Info.combine(thisField.info, thatField.info)) } } ++
      that
        .fields
        .flatMap { thatField =>
          field_(thatField.key) match {
            case None            => Some(thatField.toNonRequired)
            case Some(thatField) => None } } )

  // ===========================================================================
  def reorderKeys(f: Seq[Key] => Seq[Key], recursively: Boolean): Cls =
    copy(fields =
      keys
        .pipe(f)
        .map(field(_))
        .mapIf(_.isNesting && recursively) {
          _.transformInfo(
              _.transformNestedClass(
                  _.reorderKeys(f, recursively))) })

  // ===========================================================================
  def swapFields(parent: Option[RPath], key1: Key, key2: Key): Cls =
    parent match {
      case None    =>                           swapFields(key1, key2)
      case Some(p) => transformNestedClass(p)(_.swapFields(key1, key2)) }

  // ---------------------------------------------------------------------------
  def copyField(target: KPath, newKey: Key): Cls =
    transformx(target)(
        _.copyField(_, newKey),
        _.copyField(_, newKey))

  // ---------------------------------------------------------------------------
  def swapFields(key1: Key, key2: Key): Cls =
    fields
      .map { field =>
             if (field.key == key1) field.updateKey(key2)
        else if (field.key == key2) field.updateKey(key1)
        else                        field }
      .pipe(rewrap)

  // ---------------------------------------------------------------------------
  def copyField(key: Key, newKey: Key): Cls = rewrap(fields :+ field(key).updateKey(newKey))

  // ===========================================================================
  def zipStrings(keys: Renz, newNestingKey : Key): Cls =
      add(  key  = newNestingKey,
            info = Info.nes(nestedClass(keys)) )
        .remove(keys.froms)

    // ---------------------------------------------------------------------------
    private def nestedClass(keys: Renz): Cls =
      keys
        .values
        .map { entry =>
          field(entry.from)
            .updateKey(entry.to)
            .toNonMultiple
            .toNonRequired // TODO: t210108203819 offer a strict version
        }.pipe(Cls.apply)

  // ===========================================================================
  def untuplifyz(targetKey: Ren, newKeys: Keyz): Cls = untuplify(targetKey, newKeys, _ => Container._One) //  TODO: add check one string;
  def untuplifya(targetKey: Ren, newKeys: Keyz): Cls = untuplify(targetKey, newKeys, identity)            // TODO: add check strings
  def untuplifyb(targetKey: Ren, newKeys: Keyz): Cls = untuplify(targetKey, newKeys, _ => Container._Nes /* TODO: can it be empty? */)

    // ---------------------------------------------------------------------------
    private def untuplify(targetKey: Ren, newKeys: Keyz, f: Container => Container): Cls =
      transformInfo(targetKey) { // see 210109100250, may have to force them all to be strings
        info => Info(
            f(info.container),
            newKeys
              .map(Fld(_, Info.oneString))
              .pipe(Cls.apply)) }

  // ===========================================================================
  def unarrayEntries(newKeys: Keyz, valueKey: Key): Cls = // TODO: rename to include "pivot"
      field(valueKey)
        .pipe { valueField =>
          newKeys
            .values
            .map(valueField.updateKey)
            .map(_.toNonRequired) // pivot keys can't be required unless explicitly set so
            .pipe(Cls.apply) }

    // ---------------------------------------------------------------------------
    @deprecated("see 210303104417") def unarrayBy0(keys: Keyz, newKeys: Keyz): Cls =
      remove(keys)
        .pipe { remaining =>
          newKeys
            .values
            .map(Fld(_, Info.one(remaining)))
            .pipe(Cls.apply) }

  // ===========================================================================
  def unpivot(keyz: Keyz): Cls = {
    val rest   = self.remove(keyz)
    val target = self.retain(keyz)
    
    // ---------------------------------------------------------------------------
    val value =
      target
        .fields
        .head // validated
        .pipe { field =>                      
          cls(
            Fld.oneString(_id),                      
            field.updateKey(_vle)) }

    rest.add(Fld.nesCls(_group, value))
  }    
}

// ===========================================================================
