package gallia
package data
package single

import aptus.{Anything_, String_, Seq_}
import aptus.Separator

import data.ValueUtils
import reflect.Container
import domain.PathPair

// ===========================================================================
trait ObjOperations { self: Obj =>
  import ValueWrapper.to

  // ---------------------------------------------------------------------------
  // TODO: t210115095838 - more reliance on meta when available, to help with data, so as to minimize pattern matchings at runtime
  // note:
  // - t210104164036 and t210104164037: renaming(s) will only be applicable to the standalone version
  // - t210104164036 and t210104164037: more generally, most of this will be rewritten in that context...

  // ===========================================================================
  def modifyValuesRecursively(f: AnyValue => AnyValue): Obj =
    self
      .data
      .map { case (key, value) =>
        key ->
          (value match {
            case seq: Seq[_] =>
              seq.map {              
                case o: Obj => o.modifyValuesRecursively(f)
                case bsc    => f(bsc) }
            case o: Obj => o.modifyValuesRecursively(f)
            case bsc    => f(bsc) }) }
      .pipe(Obj.build)

  // ===========================================================================
  /** will override in this if conflict */
  def merge(that: Obj): Obj = {
    val set = that.keySet

    (self.data.filterNot(x => set.contains(x._1)) ++ that.data)
      .pipe(Obj.build0)    
  }
  
  // ===========================================================================
  def rename(x: ActualRen): Obj =    
      data
        .map { case (key, value) =>
            if (key == x.from) x.to -> value
            else               key  -> value }
        .pipe(Obj.build0)

    // ---------------------------------------------------------------------------
    def rename(from: KeyW, to: KeyW): Obj = rename(ActualRen.apply(from.value, to.value))
    def rename(x: Ren ): Obj = x.actualOpt.map(rename).getOrElse(self)
    def rename(x: Renz): Obj = x.flatMap(_.actualOpt).foldLeft(self)(_ rename _)
    //TODO: path versions?
  
  // ===========================================================================
  def remove(key1: KeyW, more: KeyW*): Obj = _remove((key1 +: more).map(_.value))
  def remove(keys: KeyWz)            : Obj = _remove(keys.keys)

  // ---------------------------------------------------------------------------
  def retain(key1: KeyW, more: KeyW*): Obj = _retain((key1 +: more).map(_.value))
  def retain(keys: KeyWz)            : Obj = _retain(keys.keys)
  
  // ---------------------------------------------------------------------------
  // TODO: optim: t210611121945 - if big enough switch to set (benchmark first)
  private def _remove(keys: Seq[Key]) : Obj = data.filterNot(x => keys.contains(x._1)).pipe(Obj.build0)
  private def _retain(keys: Seq[Key]) : Obj = data.filter   (x => keys.contains(x._1)).pipe(Obj.build0)

  // ===========================================================================
    @deprecated("very inefficient")
    private[single] def _retainPaths(paths: KPathz)         : Obj = retain(RetainMapping(paths.mapping))
                    def retain      (mapping: RetainMapping): Obj =
        data
          .flatMap { case (key, value) =>            
              mapping
                .data
                .get(key)
                .map {
                  case None              => value
                  case Some(nestedPaths) =>
                    value match {
                      case o2 : Obj    => o2                         ._retainPaths(nestedPaths)
                      case o2s: Seq[_] => o2s.map(_.asInstanceOf[Obj]._retainPaths(nestedPaths)) } }
                .map(key -> _) }
          .pipe(Obj.build)

  // ===========================================================================
  def putEntry(key:  Key, value: AnyValue) : Obj = // TODO: distinction only necessary until t201208103121 is done (Seq vs Map for Obj)
      if (containsKey(key)) replaceEntry(key, value)
      else                      addEntry(key, value)
    def putEntry(key: KeyW, value: AnyValue) : Obj = putEntry(key.value, value)  // must come *after* the non-KeyW version

    // ---------------------------------------------------------------------------
    def putEntries(entries: Seq[(KPath, AnyValue)]): Obj = // TODO: quite inefficient
      entries.foldLeft(self){ case (curr, (path, value)) =>
        curr.putPath(path, value) }

    // ---------------------------------------------------------------------------
    def putEntries(entry1: DataEntry, entry2: DataEntry, more: DataEntry*): Obj = putEntries(Seq(entry1, entry2) ++ more) // TODO: quite inefficient
    def putEntries(entries: Seq[DataEntry])(implicit di: DI)              : Obj =                                         // TODO: quite inefficient
      entries.foldLeft(self){ case (curr, entry) =>
        curr.putEntry(entry.key, entry.value) }

  // ---------------------------------------------------------------------------
  def addEntry(key: Key , value: AnyValue): Obj = Obj.build(data :+ (key -> value))
  def addEntry(key: KeyW, value: AnyValue): Obj = addEntry(key.value, value) // must come *after* the non-KeyW version

    // ---------------------------------------------------------------------------
    def addEntries(entry1: DataEntry, entry2: DataEntry, more: DataEntry*): Obj = addEntries(Seq(entry1, entry2) ++ more) // TODO: quite inefficient
    def addEntries(entries: Seq[DataEntry])(implicit di: DI)              : Obj =                                         // TODO: quite inefficient
      entries.foldLeft(self){ case (curr, entry) =>
        curr.addEntry(entry.key, entry.value) }

  // ---------------------------------------------------------------------------
  def replaceEntry(key: Key, value: AnyValue) : Obj =
        data
          .map { case (k, v) => k ->
            (if (key == k) value
             else          v ) }
          .pipe(Obj.build)

      def replaceEntry(key: KeyW, value: AnyValue) : Obj = replaceEntry(key.value, value) // must come *after* the non-KeyW version

    // ---------------------------------------------------------------------------
    def replaceEntries(entry1: DataEntry, entry2: DataEntry, more: DataEntry*): Obj = replaceEntries(Seq(entry1, entry2) ++ more) // TODO: quite inefficient
    def replaceEntries(entries: Seq[DataEntry])(implicit di: DI)              : Obj =                                         // TODO: quite inefficient
      entries.foldLeft(self){ case (curr, entry) =>
        curr.replaceEntry(entry.key, entry.value) }

  // ===========================================================================
  def addPath(target: KPathW, value: AnyValue): Obj =
      target.value.initPair match {
        case (None      , leaf) =>                             addEntry(leaf, value)
        case (Some(tail), leaf) => transformPath(tail, _.asObj.addEntry(leaf, value)) }

    // ---------------------------------------------------------------------------
    def replacePath(target: KPathW, value: AnyValue): Obj =
      target.value.initPair match {
        case (None      , leaf) =>                             replaceEntry(leaf, value)
        case (Some(tail), leaf) => transformPath(tail, _.asObj.replaceEntry(leaf, value)) }

    // ---------------------------------------------------------------------------
    def putPath(target: KPathW, value: AnyValue): Obj =
      target.value.initPair match {
        case (None      , leaf) =>                             putEntry(leaf, value)
        case (Some(tail), leaf) => transformPath(tail, _.asObj.putEntry(leaf, value)) }

  // ===========================================================================
  def transformPath(target: KPathW, f: AnyValue => AnyValue): Obj = // TODO: phase out
      target.value.tailPair match {
          case (leaf  , None      ) => transformKey(leaf, f)
          case (parent, Some(tail)) =>
            (attemptKey(parent) match {
              case None        => self
              case Some(value) => replaceEntry(parent, value match { // TODO: could use meta
                  case seq: Seq[_] => seq.asInstanceOf[Seq[Obj]].map(_.transformPath(tail, f))
                  case sgl         => sgl.asInstanceOf[    Obj ]      .transformPath(tail, f) }) }) }

    // ---------------------------------------------------------------------------
    def transformPathx(target: KPathW, f: AnyValue => AnyValue): Obj =
      target.value.tailPair match {
          case (leaf  , None      ) => transformKeyx(leaf, f)
          case (parent, Some(tail)) =>
            (attemptKey(parent) match {
              case None        => self
              case Some(value) => replaceEntry(parent, value match { // TODO: could use meta
                  case seq: Seq[_] => seq.asInstanceOf[Seq[Obj]].map(_.transformPathx(tail, f))
                  case sgl         => sgl.asInstanceOf[    Obj ]      .transformPathx(tail, f) }) }) }

    // ---------------------------------------------------------------------------
    def transformPathPair(target: PathPair, f: AnyValue => AnyValue): Obj =
      target.path.tailPair match {
          case (leaf  , None       ) => transformKeyPair(leaf, target.optional)(f)
          case (parent, Some(tail0)) =>
            val tail = PathPair(tail0, target.optional)
            (attemptKey(parent) match {
              case None        => self
              case Some(value) => replaceEntry(parent, value match { // TODO: could use meta
                  case seq: Seq[_] => seq.asInstanceOf[Seq[Obj]].map(_.transformPathPair(tail, f))
                  case sgl         => sgl.asInstanceOf[    Obj ]      .transformPathPair(tail, f) }) }) }

    // ---------------------------------------------------------------------------
    // only for Whatever to Whatever transformation...
    def transformWhateverPathPair(target: PathPair, f: AnyValue => AnyValue, checkType: Boolean): Obj =
        target.path.tailPair match {
            case (leaf  , None       ) => transformWhateverKeyPair(leaf, target.optional, checkType)(f)
            case (parent, Some(tail0)) =>
              val tail = PathPair(tail0, target.optional)
              (attemptKey(parent) match {
                case None        => self
                case Some(value) => replaceEntry(parent, value match { // TODO: could use meta
                    case seq: Seq[_] => seq.asInstanceOf[Seq[Obj]].map(_.transformWhateverPathPair(tail, f, checkType))
                    case sgl         => sgl.asInstanceOf[    Obj ]      .transformWhateverPathPair(tail, f, checkType) }) }) }

      // ===========================================================================
      def transformKey(key: Key, f: AnyValue => AnyValue): Obj = // TODO: phase out
          attemptKey(key)
            .map { value => putEntry(key, f(value)) }
            .getOrElse(self)

      // ---------------------------------------------------------------------------
      def transformKeyx(key: Key, f: AnyValue => AnyValue): Obj = transformRenx(Ren.from(key))(f)
      def transformRenx(key: Ren)(f: AnyValue => AnyValue): Obj =
          attemptKey(key.from) // TODO: could use meta
            .map {
              case seq: Seq[_] => putEntry(key.from, seq.map(f))
              case sgl         => putEntry(key.from, f(sgl)) }
            .map(_.rename(key))
            .getOrElse(self)

      // ---------------------------------------------------------------------------
      def transformKeyPair(key: Key, optional: Boolean)(f: AnyValue => AnyValue): Obj =
        (if (optional) attemptKey(key) // TODO: could use meta
         else          attemptKey(key).getOrElse(aptus.illegalState(key, optional, self.formatPrettyJson.take(1000))))
          .pipe(f)
          .pipe(putEntry(key, _))

      // ---------------------------------------------------------------------------
      // abstracts requiredness + optionally check resulting type
      def transformWhateverKeyPair(key: Key, optional: Boolean, checkType: Boolean)(f: AnyValue => AnyValue): Obj =
        if (optional)
          attemptKey(key)
            .map(_computeNewValue(f, checkType))
            .map(putEntry(key, _))
            .getOrElse(self)
        else
          attemptKey(key).get
            .pipe(_computeNewValue(f, checkType))
            .pipe(putEntry(key, _))

      // ---------------------------------------------------------------------------
      private def _computeNewValue(f: AnyValue => AnyValue, checkType: Boolean)(value: Any): Any = {
        val newValue = f(value)
        if (checkType) ValueUtils.checkSameTypes(value, newValue)

        newValue
      }

    // ===========================================================================
    def attemptPath(target: KPathW): Option[AnyValue] =
      target.value.tailPair match {
        case (leaf  , None      ) => attemptKey(leaf)
        case (parent, Some(tail)) =>
          attemptKey(parent) match {
              case None => None
              case Some(value) =>
                value match { // TODO: could use meta (see t210115095838)
                  case _: Seq[_] => dataError(s"TODO:CantBeSeq-Opt:210106171801:${target}") // in theory should have been validated against..
                  case sgl       => sgl.asInstanceOf[Obj].attemptPath(tail) } } }

    // ---------------------------------------------------------------------------
    def forcePath(target: KPathW): AnyValue  =
      target.value.tailPair match {
        case (leaf  , None      ) => forceKey(leaf)
        case (parent, Some(tail)) =>
          attemptKey(parent) match {
              case None        => dataError(s"TODO:CantBeNone:210106171759:${target}") // in theory should have been validated against..
              case Some(value) =>
                value match { // TODO: could use meta (see t210115095838)
                  case _: Seq[_] => dataError(s"TODO:CantBeSeq-Force:210106171800:${target}") // in theory should have been validated against..
                  case sgl       => sgl.asInstanceOf[Obj].forcePath(tail) } } }

    // ---------------------------------------------------------------------------
    def containsPath(target: KPathW): Boolean =
      target.value.tailPair match {
        case (leaf  , None      ) => containsKey(leaf)
        case (parent, Some(tail)) =>
          attemptKey(parent) match {
              case None        => false
              case Some(value) =>
                (value match {
                    case seq: Seq[_] => seq
                    case sgl         => Seq(sgl) })
                  .forall(_.asInstanceOf[Obj].containsPath(tail)) } }
    
    // ===========================================================================
    // TODO: t210116165405 - benchmark, which is faster?
    @deprecated def seq0(target: KPathW, optional: Boolean, multiple: Boolean): Seq[AnyValue] =
        (optional, multiple) match {
          case (true , true ) => attemptPath(target).toSeq.flatMap(_.asSeq)
          case (true , false) => attemptPath(target).toSeq
          case (false, true ) => forcePath  (target)                .asSeq
          case (false, false) => forcePath  (target).in.seq }

      // ===========================================================================
      def seq(target: KPathW, container: Container): Seq[AnyValue] =
        container match {
          case Container._One => forcePath  (target).in.seq
          case Container._Opt => attemptPath(target).toSeq
          case Container._Nes => forcePath  (target)                .asSeq
          case Container._Pes => attemptPath(target).toSeq.flatMap(_.asSeq) }

  // ===========================================================================
  def transformObj   (key: Key, f: Obj    => Any): Obj = transformPath(key, _.asObj   .pipe(f))
  def transformObjx  (key: Key, f: Obj    => Any): Obj = transformPath(key,
      _ match { // TODO: should use schema rather (see t210115095838)
        case x: Seq[_] => x.map(_.asObj.pipe(f))
        case x         => x      .asObj.pipe(f) } )

  // ===========================================================================
  // ===========================================================================
  // ===========================================================================
  // more advanced

  def swapEntries(key1: Key, key2: Key): Obj =
    (containsKey(key1), containsKey(key2)) match {
      case (false, false) => self
      case (true , false) => rename      (key1, key2)
      case (false, true ) => rename      (key2, key1)
      case (true , true ) =>
        data
          .map { case (key, value) =>
                 if (key == key1) key2 -> value
            else if (key == key2) key1 -> value
            else                  key  -> value }
          .pipe(Obj.build) }  

  // ---------------------------------------------------------------------------
  def copyEntries(original: Key, newKey: Key): Obj =
    attemptKey(original) match {
      case None    => self
      case Some(x) => addEntry(newKey, x) }

  // ---------------------------------------------------------------------------
  def nest(target: Key, nestingKey: Key): Obj = nest(Keyz.from(target), nestingKey)

    /* req: nesting key can't be an array */
    def nest(targets: Keyz, nestingKey: Key): Obj =
      (retainOpt(targets), removeOpt(targets)) match {
        case (None        , _         ) => self
        case (Some(target), None      ) => gallia.obj(nestingKey -> target)
        case (Some(target), Some(rest)) =>
          attemptKey(nestingKey)
            .map {
              case seq: Seq[_] => rest.putEntry(nestingKey, seq.map(_.asObj.merge(target))) // note: denormalizes
              case sgl         => rest.putEntry(nestingKey,       sgl.asObj.merge(target)) }
            .getOrElse {          rest.putEntry(nestingKey,                       target ) } }

  // ---------------------------------------------------------------------------
  def split(key: Key, splitter: String => Seq[String]): Obj = transformKey(key, _.toString.pipe(splitter.apply))

  // ---------------------------------------------------------------------------
  @deprecated("still needed after 210303101932?") def unarrayCompositeKey(keys: Seq[Key], separator: Separator): Option[Key] =
    keys
      .flatMap { keyKey => attemptKey(keyKey).map(_.str) } /* TODO or expect default values to be set if missing? or ignore collisions? */
      .in.noneIf(_.isEmpty)
      .map(_.join(separator).symbol)

  // ---------------------------------------------------------------------------
  def unarrayCompositeKey2(key: Key): Option[Key] =
    attemptKey(key)
      .flatMap(_.str.in.noneIf(_.isEmpty)) /* TODO or expect default values to be set if missing? or ignore collisions? */      
      .map(_.symbol)      

  // ---------------------------------------------------------------------------
  def unpivot(keyz: Keyz): Obj = {      
    val rest   = self.removeOpt(keyz).get
    val target = self.retainOpt(keyz).get
    
    val value =
      target
        .entries
        .map { case (k, v) => 
          gallia.obj(_id -> k.name, _vle -> v) }

    rest.addEntry(_group, value)
  }

  // ===========================================================================
  def removeIf(reference: Key, target: Key, pred: AnyValue => Boolean): Obj =
      attemptKey(reference) // reference may be the same as target (most common case)
        .map { value =>
          if (pred(value)) remove(target)
          else             self }
        .getOrElse(self)

    // ---------------------------------------------------------------------------
    def removeWhateverIf(key: Key, targetValue: AnyValue): Obj =
      attemptKey(key)
        .map { value =>
          if (value == targetValue) remove(key)
          else                      self }
        .getOrElse(self)

    // ---------------------------------------------------------------------------
    def removeWhateverIfAll(map: Map[Key, AnyValue]): Obj =
      data
        .flatMap { case (key, value) =>
          map.get(key) match {
            case None              =>                                     Some(key -> value)
            case Some(targetValue) => if (value == targetValue) None else Some(key -> value) } }
        .pipe(Obj.build0)

  // ---------------------------------------------------------------------------
  def setDefault2(reference: Key, target: Key, pred: AnyValue => Boolean, newValue: AnyValue): Obj =
    attemptKey(reference)
      .map { value =>
        if (pred(value) && !containsKey(target)) addEntry(target, newValue)
        else                                     self }
      .getOrElse(self)
}

// ===========================================================================
