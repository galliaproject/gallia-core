package gallia
package data
package single

import meta.{Container, BasicType}
import domain.PathPair
import domain.SortingPair

// ===========================================================================
object ObjOrdering {

  def oneObjOrdering(c: Cls, pair: SortingPair): Ordering[Obj] =
    new Ordering[Obj] {

      def compare(left: Obj, right: Obj): Int = // TODO: worth trying to optimize? (eg trivial cases, ...)
          //TODO: just use Iterable's?
          c .fields
              .view
              .map(compareField(left, right))
              .find(_ != 0) // TODO: comfirm lazy
              .getOrElse(0)

       // ---------------------------------------------------------------------------
       private def compareField(left: Obj, right: Obj)(field: Fld): Int = {
            val key      = field.key
            val optional = field.isOptional
            
            field
              .info
              .union
              .view
              .map(compareSubInfo(key, optional)(left, right))
              .find(_ != 0)        
              .getOrElse(0)
          }

          // ---------------------------------------------------------------------------
          private def compareSubInfo(key: Key, optional: Boolean)(left: Obj, right: Obj)(subInfo: meta.SubInfo): Int =                 
            subInfo.valueType match {
  
              // ---------------------------------------------------------------------------
              case tipe: BasicType =>
                val ori = PathPair(KPath.from(key), optional)
                val container = reflect.Container.from(optional, subInfo.multiple)

                tipe.compare(container, pair.descending, pair.missingLast)(
                    ori.lookup(left),
                    ori.lookup(right))
  
              // ---------------------------------------------------------------------------
              case nc: Cls =>
                val ori      : PathPair  = PathPair(KPath.from(key), optional)
                val container: Container = reflect.Container.from(optional, subInfo.multiple)

                container match {
                  case Container._One =>
                    oneObjOrdering(nc, pair)
                      .compare(
                        ori.lookup(left) .asInstanceOf[Obj],
                        ori.lookup(right).asInstanceOf[Obj])
                  case Container._Opt =>
                    optObjOrdering(nc, pair)
                      .compare(
                        ori.lookup(left) .asInstanceOf[Option[Obj]],
                        ori.lookup(right).asInstanceOf[Option[Obj]])
                  case Container._Nes =>
                    nesObjOrdering(nc, pair)
                      .compare(
                        ori.lookup(left) .asInstanceOf[Seq[Obj]],
                        ori.lookup(right).asInstanceOf[Seq[Obj]])
                  case Container._Pes =>
                    pesObjOrdering(nc, pair)
                      .compare(
                        ori.lookup(left) .asInstanceOf[Option[Seq[Obj]]],
                        ori.lookup(right).asInstanceOf[Option[Seq[Obj]]]) } } }

  // ===========================================================================
  def optObjOrdering(c: Cls, pair: SortingPair): Ordering[Option[Obj]] = {
    implicit val ord: Ordering[Obj] = oneObjOrdering(c, pair)
    implicitly[Ordering[Option[Obj]]]
  }

  // ---------------------------------------------------------------------------
  def nesObjOrdering(c: Cls, pair: SortingPair): Ordering[Seq[Obj]] = {
    implicit val ord1: Ordering[    Obj ] = oneObjOrdering(c, pair)
    implicit val ord2: Ordering[Seq[Obj]] = aptus.seqOrdering
    implicitly[Ordering[Seq[Obj]]]
  }

  // ---------------------------------------------------------------------------
  def pesObjOrdering(c: Cls, pair: SortingPair): Ordering[Option[Seq[Obj]]] = {
    implicit val ord1: Ordering[    Obj ] = oneObjOrdering(c, pair)
    implicit val ord2: Ordering[Seq[Obj]] = aptus.seqOrdering
    implicitly[Ordering[Option[Seq[Obj]]]]
  }

}

// ===========================================================================
