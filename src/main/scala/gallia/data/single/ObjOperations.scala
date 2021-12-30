package gallia
package data.single

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
  def merge(that: Obj): Obj = {
    val set = that.keySet

    (self.data.filterNot(_._1.containedIn(set)) ++ that.data)
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
  private def _remove(keys: Seq[Key]) : Obj = data.filterNot(_._1.containedIn(keys)).pipe(Obj.build0)
  private def _retain(keys: Seq[Key]) : Obj = data.filter   (_._1.containedIn(keys)).pipe(Obj.build0)    

  // ===========================================================================
    @deprecated("very inefficient")
    private[single] def _retainPaths(paths: KPathz)                        : Obj = retain(paths, RetainMapping(paths.mapping))
                    def retain      (paths: KPathz, mapping: RetainMapping): Obj =    
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
  private def _put(key: Key, value: AnyValue) : Obj = // TODO: distinction only necessary until t201208103121 is done (Seq vs Map for Obj)
        if (containsKey(key)) _replace(key, value)
        else                  _add    (key, value)

      // ---------------------------------------------------------------------------
      private def _add    (key: Key, value: AnyValue) : Obj = Obj.build(data :+ (key -> value))
      private def _replace(key: Key, value: AnyValue) : Obj =
          data
            .map { case (k, v) => k ->
              (if (key == k) value
               else          v ) }
            .pipe(Obj.build)

    // ---------------------------------------------------------------------------
    def add(target: KPathW, value: AnyValue): Obj =
      target.value.initPair match {
        case (None      , leaf) =>                            _add(leaf, value)
        case (Some(tail), leaf) => transformPath(tail, _.asObj.add(leaf, value)) }

    def replace(target: KPathW, value: AnyValue): Obj =
      target.value.initPair match {
        case (None      , leaf) =>                            _replace(leaf, value)
        case (Some(tail), leaf) => transformPath(tail, _.asObj.replace(leaf, value)) }

    def put(target: KPathW, value: AnyValue): Obj =
      target.value.initPair match {
        case (None      , leaf) =>                            _put(leaf, value)
        case (Some(tail), leaf) => transformPath(tail, _.asObj.put(leaf, value)) }

    // ---------------------------------------------------------------------------
    def put(entries: Seq[(KPath, AnyValue)]): Obj = // TODO: quite inefficient
      entries.foldLeft(self){ case (curr, (path, value)) =>
        curr.put(path, value) }

  // ===========================================================================
  def transformPath(target: KPathW, f: AnyValue => AnyValue): Obj = // TODO: phase out
      target.value.tailPair match {
          case (leaf  , None      ) => _transformKey(leaf, f)
          case (parent, Some(tail)) =>
            (attemptKey(parent) match {
              case None        => self
              case Some(value) => replace(parent, value match { // TODO: could use meta
                  case seq: Seq[_] => seq.asInstanceOf[Seq[Obj]].map(_.transformPath(tail, f))
                  case sgl         => sgl.asInstanceOf[    Obj ]      .transformPath(tail, f) }) }) }

    // ---------------------------------------------------------------------------
    def transformPathx(target: KPathW, f: AnyValue => AnyValue): Obj =
      target.value.tailPair match {
          case (leaf  , None      ) => _transformKeyx(leaf, f)
          case (parent, Some(tail)) =>
            (attemptKey(parent) match {
              case None        => self
              case Some(value) => replace(parent, value match { // TODO: could use meta
                  case seq: Seq[_] => seq.asInstanceOf[Seq[Obj]].map(_.transformPathx(tail, f))
                  case sgl         => sgl.asInstanceOf[    Obj ]      .transformPathx(tail, f) }) }) }

    // ---------------------------------------------------------------------------
    def transformPathPair(target: PathPair, f: AnyValue => AnyValue): Obj =
      target.path.tailPair match {
          case (leaf  , None       ) => _transformKeyPair(leaf, target.optional)(f)
          case (parent, Some(tail0)) =>
            val tail = PathPair(tail0, target.optional)
            (attemptKey(parent) match {
              case None        => self
              case Some(value) => replace(parent, value match { // TODO: could use meta
                  case seq: Seq[_] => seq.asInstanceOf[Seq[Obj]].map(_.transformPathPair(tail, f))
                  case sgl         => sgl.asInstanceOf[    Obj ]      .transformPathPair(tail, f) }) }) }

    // ---------------------------------------------------------------------------
    // only for Whatever to Whatever transformation...
    def transformWhateverPathPair(target: PathPair, f: AnyValue => AnyValue, checkType: Boolean): Obj =
      target.path.tailPair match {
          case (leaf  , None       ) => _transformWhateverKeyPair(leaf, target.optional, checkType)(f)
          case (parent, Some(tail0)) =>
            val tail = PathPair(tail0, target.optional)
            (attemptKey(parent) match {
              case None        => self
              case Some(value) => replace(parent, value match { // TODO: could use meta
                  case seq: Seq[_] => seq.asInstanceOf[Seq[Obj]].map(_.transformWhateverPathPair(tail, f, checkType))
                  case sgl         => sgl.asInstanceOf[    Obj ]      .transformWhateverPathPair(tail, f, checkType) }) }) }
    
      // ===========================================================================
      def _transformKey(key: Key, f: AnyValue => AnyValue): Obj = // TODO: phase out
          attemptKey(key)
            .map { value => _put(key, f(value)) }
            .getOrElse(self)

      // ---------------------------------------------------------------------------
      def _transformKeyx(key: Key, f: AnyValue => AnyValue): Obj = _transformRenx(Ren.from(key))(f)
      def _transformRenx(key: Ren)(f: AnyValue => AnyValue): Obj =
          attemptKey(key.from) // TODO: could use meta
            .map {
              case seq: Seq[_] => _put(key.from, seq.map(f))
              case sgl         => _put(key.from, f(sgl)) }
            .map(_.rename(key))
            .getOrElse(self)

      // ---------------------------------------------------------------------------
      def _transformKeyPair(key: Key, optional: Boolean)(f: AnyValue => AnyValue): Obj =
        (if (optional) attemptKey(key) // TODO: could use meta
         else          attemptKey(key).get)
          .pipe(f)
          .pipe(_put(key, _))

      // ---------------------------------------------------------------------------
      // abstracts requiredness + optionally check resulting type
      def _transformWhateverKeyPair(key: Key, optional: Boolean, checkType: Boolean)(f: AnyValue => AnyValue): Obj =
          if (optional)
            attemptKey(key)
              .map(_computeNewValue(f, checkType))
              .map(_put(key, _))
              .getOrElse(self)            
          else
            attemptKey(key).get
              .pipe(_computeNewValue(f, checkType))
              .pipe(_put(key, _))        

        // ---------------------------------------------------------------------------
        private def _computeNewValue(f: AnyValue => AnyValue, checkType: Boolean)(value: Any): Any = {
          val newValue = f(value)
          if (checkType) ValueUtils.checkSameTypes(value, newValue)

          newValue            
        }

    // ===========================================================================
    def opt(target: KPathW): Option[AnyValue] =
      target.value.tailPair match {
        case (leaf  , None      ) => attemptKey(leaf)
        case (parent, Some(tail)) =>
          attemptKey(parent) match {
              case None => None
              case Some(value) =>
                value match { // TODO: could use meta (see t210115095838)
                  case seq: Seq[_] => dataError(s"TODO:CantBeSeq-Opt:210106171801:${target}") // in theory should have been validated against..
                  case sgl         => sgl.asInstanceOf[Obj].opt(tail) } } }

    // ---------------------------------------------------------------------------
    def force(target: KPathW): AnyValue  =
      target.value.tailPair match {
        case (leaf  , None      ) => attemptKey(leaf).get
        case (parent, Some(tail)) =>
          attemptKey(parent) match {
              case None        => dataError(s"TODO:CantBeNone:210106171759:${target}") // in theory should have been validated against..
              case Some(value) =>
                value match { // TODO: could use meta (see t210115095838)
                  case seq: Seq[_] => dataError(s"TODO:CantBeSeq-Force:210106171800:${target}") // in theory should have been validated against..
                  case sgl         => sgl.asInstanceOf[Obj].force(tail) } } }

    // ---------------------------------------------------------------------------
    private[single] def _contains(target: KPathW): Boolean =
      target.value.tailPair match {
        case (leaf  , None      ) => containsKey(leaf)
        case (parent, Some(tail)) =>
          attemptKey(parent) match {
              case None        => false
              case Some(value) =>
                (value match {
                    case seq: Seq[_] => seq
                    case sgl         => Seq(sgl) })
                  .forall(_.asInstanceOf[Obj]._contains(tail)) } }   
    
    // ===========================================================================
    // TODO: t210116165405 - benchmark, which is faster?
    @deprecated def seq0(target: KPathW, optional: Boolean, multiple: Boolean): Seq[AnyValue] =
        (optional, multiple) match {
          case (true , true ) => opt  (target).toSeq.flatMap(_.asSeq)
          case (true , false) => opt  (target).toSeq
          case (false, true ) => force(target)                .asSeq
          case (false, false) => force(target).in.seq }

      // ===========================================================================
      def seq(target: KPathW, container: Container): Seq[AnyValue] =
        container match {
          case Container._One => force(target).in.seq // TODO: see t210116165559 - rename to "in"?
          case Container._Opt => opt  (target).toSeq
          case Container._Nes => force(target)                .asSeq
          case Container._Pes => opt  (target).toSeq.flatMap(_.asSeq) }

  // ===========================================================================
  def toUpperCase(target: Key): Obj = _transformKey(target, _.asString.toUpperCase)
  //TODO: more... (see t210104164037)

  // ===========================================================================
  def transformString (key: Key, f: String      => Any): Obj = transformPath(key, _.asString             .pipe(f))
  def transformInts   (key: Key, f: Seq[Int   ] => Any): Obj = transformPath(key, _.asSeq.map(_.asInt   ).pipe(f))
  def transformDoubles(key: Key, f: Seq[Double] => Any): Obj = transformPath(key, _.asSeq.map(_.asDouble).pipe(f))

  // ---------------------------------------------------------------------------
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
    (contains(key1),contains(key2)) match {
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
      case Some(x) => _add(newKey, x) }

  // ---------------------------------------------------------------------------
  def nest(target: Key, nestingKey: Key): Obj = nest(Keyz.from(target), nestingKey)

    /* req: nesting key can't be an array */
    def nest(targets: Keyz, nestingKey: Key): Obj =
      (retainOpt(targets), removeOpt(targets)) match {
        case (None        , _         ) => self
        case (Some(target), None      ) => gallia.obj(nestingKey -> target)
        case (Some(target), Some(rest)) =>
          attemptKey(nestingKey) match {
            case None                   => rest.put(nestingKey,                      target )
            case Some(existing)         => rest.put(nestingKey, existing.asObj.merge(target)) } }

  // ---------------------------------------------------------------------------
  def split(key: Key, splitter: String => Seq[String]): Obj = _transformKey(key, _.toString.pipe(splitter.apply))

  // ---------------------------------------------------------------------------
  @deprecated("still needed after 210303101932?") def unarrayCompositeKey(keys: Seq[Key], separator: Separator): Option[Key] =
    keys
      .flatMap { keyKey => opt(keyKey).map(_.str) } /* TODO or expect default values to be set if missing? or ignore collisions? */
      .in.noneIf(_.isEmpty)
      .map(_.join(separator).symbol)

  // ---------------------------------------------------------------------------
  def unarrayCompositeKey2(key: Key): Option[Key] =
    opt(key)
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

    rest.add(_group, value)        
  }

}

// ===========================================================================
