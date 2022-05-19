package gallia
package atoms
package common

import aptus.Seq_

// ===========================================================================
@deprecated("see t210122151934") object AtomsUUReducer0 {

  case class _ToSize(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[_]].size) }

    // ---------------------------------------------------------------------------  
    case class _ToIntSum   (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Int]]   .sum) }
    case class _ToDoubleSum(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Double]].sum) }
    
    case class _ToByteSum  (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Byte]]  .sum) }
    case class _ToShortSum (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Short]] .sum) }
    case class _ToLongSum  (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Long]]  .sum) }
    case class _ToFloatSum (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Float]] .sum) }    
    
    case class _ToBigIntSum(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[BigInt]].sum) }    
    case class _ToBigDecSum(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[BigDec]].sum) }

    // ---------------------------------------------------------------------------
    case class _ToIntMean   (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Int]]   .mean) }
    case class _ToDoubleMean(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Double]].mean) }
    
    case class _ToByteMean  (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Byte]]  .mean) }
    case class _ToShortMean (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Short]] .mean) }
    case class _ToLongMean  (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Long]]  .mean) }
    case class _ToFloatMean (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Float]] .mean) }    
    
    case class _ToBigIntMean(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[BigInt]].mean) }    
    case class _ToBigDecMean(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[BigDec]].mean) }
    
    // ---------------------------------------------------------------------------
    case class _ToIntStdev   (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Int]]   .stdev) }
    case class _ToDoubleStdev(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Double]].stdev) }
    
    case class _ToByteStdev  (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Byte]]  .stdev) }
    case class _ToShortStdev (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Short]] .stdev) }
    case class _ToLongStdev  (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Long]]  .stdev) }
    case class _ToFloatStdev (key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[Float]] .stdev) }    
    
    case class _ToBigIntStdev(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[BigInt]].stdev) }    
    case class _ToBigDecStdev(key: Key) extends AtomUU { def naive(o: Obj) = o.transformPath(key, _.asInstanceOf[Seq[BigDec]].stdev) }
}

// ===========================================================================

