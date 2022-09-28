package gallia
package atoms
package obg9

import gallia.reflect.Container

// ===========================================================================
object Obg9Conversions {

  def fromObj(c1: Cls)(o: Obj): Obg9 = {
      val size                  = c1.size
      val lookup: Map[Key, Any] = o.lookup   
      val data: Array[Any] = new Array[Any](size)
      
        c1
          .fields
          .zipWithIndex
          .foreach { case (field, index) =>            
            data(index) = value(field)(lookup, field.key) }
      
      new Obg9(size, data)
    }
  
    // ---------------------------------------------------------------------------
    private def value(field: Fld)(lookup: Map[Key, Any], key: Key): Any =
      field.nestedClassOpt match {
        case None     => lookup.get(key).getOrElse(null)
        case Some(nc) =>
          field.container1 match {
            case Container._One => lookup.apply(key).asInstanceOf[           Obj  ].pipe(      fromObj(nc))
            case Container._Opt => lookup.get  (key).asInstanceOf[Option[    Obj ]].map (      fromObj(nc)) .getOrElse(null)
            case Container._Nes => lookup.apply(key).asInstanceOf[       Seq[Obj] ]       .map(fromObj(nc))
            case Container._Pes => lookup.get  (key).asInstanceOf[Option[Seq[Obj]]].map (_.map(fromObj(nc))).getOrElse(null) } }  
  
  // ===========================================================================
  def backToObj(c2: Cls)(o: Obg9): Obj =
    c2
      .fields
      .zipWithIndex
      .flatMap { case (field, index) =>
        val value = o.data(index)
        
        if (value == null) None
        else Some(
        field.key ->
          (field.nestedClassOpt match {
            case None     => value
            case Some(nc) =>   
              field.container1 match {
                case Container._One | Container._Opt => value .asInstanceOf[           Obg9  ].pipe      (backToObj(nc))
                case Container._Nes | Container._Pes => value .asInstanceOf[       Seq[Obg9] ].       map(backToObj(nc)) } })) }
      .pipe { x => Obj.build0(x.toArray) }

}

// ===========================================================================
