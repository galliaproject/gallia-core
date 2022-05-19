package gallia
package meta

import aptus.{Anything_, Seq_}

// ===========================================================================
trait ClsNestingRelated { self: Cls =>

  def nest(targetKey : Ren , nestingKey: Key): Cls = nest(Renz(Seq(targetKey)), nestingKey)
  def nest(targetKeys: Renz, nestingKey: Key): Cls =
    if (keySet.contains(nestingKey)) nestInto (targetKeys, nestingKey)
    else                             nestUnder(targetKeys, nestingKey)

  // ---------------------------------------------------------------------------
  private def nestInto (targetKeys: Renz, existingNestingKey: Key): Cls =
    replace(
        key  = Ren.from(existingNestingKey),
        info = updatedNestingFieldInfo(this, targetKeys, existingNestingKey))
      .remove(targetKeys.froms)

  // ---------------------------------------------------------------------------
  private def nestUnder(targetKeys: Renz, newNestingKey: Key): Cls =
    add(
        key  = newNestingKey,
        info = newNestingFieldInfo(this, targetKeys))
      .remove(targetKeys.froms)

  // ===========================================================================
  def unnestObject(key: Key): Cls = //TODO: add req
    potch0(key)
      .pipe { case (in, out) =>
        out.mergeDisjoint(
          in
            .fields.force.one
            .forceNestedClass) }

    // ---------------------------------------------------------------------------
    def unnestOOO(key: Key): Cls = //TODO: add req
      potch0(key)
        .pipe { case (in, out) =>
          out.mergeDisjoint(
            in
              .fields.force.one
              .forceNestedClass) }

    // ---------------------------------------------------------------------------
    def unnestObject(path: KPath): Cls =
      transformx(path)(
          root = _ unnestObject _,
          rec  = _ unnestObject _)

    // ---------------------------------------------------------------------------
    def unnestField(parent: KPath, leaf: Key): Cls =
        transformx(parent)(
            root = _ unnestField(_, leaf),
            rec  = _ unnestField(_, leaf))

      // ---------------------------------------------------------------------------
      private[meta] def unnestField(parentKey: Key, leaf: Key): Cls = {
        val nestedClass = field(parentKey).forceNestedClass

        val (nestedField, remainingNestedClassOpt) = nestedClass.potchSingle(leaf)

        add(nestedField).pipe { x =>
          remainingNestedClassOpt match {
            case None                       => x.remove(parentKey)
            case Some(remainingNestedClass) =>
              x.transformSoleSubInfo(parentKey) {
                  _.updateValueType(remainingNestedClass) } } }
      }

  // ===========================================================================
  def updatedNestingFieldInfo(c: Cls, targetKeys: Renz, existingNestingKey: Key): Info = { // for nest into
    val existingNestingFieldInfo: Info = c.field(existingNestingKey).info

    val existingNestedFields: Seq[Fld] =
      existingNestingFieldInfo
        .forceNestedClass
        .fields

    val updatedNestedClass: Cls = Cls(
      existingNestedFields ++
        nestingFields(c, targetKeys))

    existingNestingFieldInfo
      .updateValueType(updatedNestedClass)
    //FIXME: t210122162650 - p1 - meta nest into: handle optional like nest under
    //.pipe { subInfo =>
    //  val nestedKeys: Keyz = combineNestedKeys(targetKeys, existingNestedFields)
    //  if (c.areAllNonRequired(nestedKeys)) subInfo.toOptional
    //  else                                 subInfo }
  }

  // ===========================================================================
  def newNestingFieldInfo(c: Cls, targetKeys: Renz): Info = // for nest under
    nestingFields(c, targetKeys)
      .pipe(Cls.apply)
      .pipe { nestedClass =>
        if (c.areAllNonRequired(targetKeys.froms)) Info.opt(nestedClass)
        else                                       Info.one(nestedClass) }

  // ===========================================================================
  private def nestingFields(c: Cls, targetKeys: Renz): Seq[Fld] =
    targetKeys.map { entry =>
      c .field(    entry.from)
        .updateKey(entry.to) }

  // ===========================================================================
  // TODO: t210124100009 - name
  @deprecated private     def potch0     (key: RenW): (Cls, Cls)         = retain(key.value) -> remove   (key.from) // cool kids know what potch means
  @deprecated /*private */def potchSingle(key: RenW): (Fld, Option[Cls]) = field(key.value) -> removeOpt(key.from)

  // ---------------------------------------------------------------------------
  @deprecated private def removeOpt(target : Key ): Option[Cls] = { requireKnownKey (target ); fields.filterNot(_.key == target).in.noneIf(_.isEmpty).map(rewrap) }
}

// ===========================================================================
