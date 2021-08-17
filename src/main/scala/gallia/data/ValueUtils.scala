package gallia.data

import aptus.Tuple2_ // for toOptionalTuple
import gallia.reflect.Container

// ===========================================================================
object ValueUtils {

  def checkSameTypes(from: Any, to: Any): Unit = {    
    val pairOpt1 = Container.containerPairOpt(from)
    val pairOpt2 = Container.containerPairOpt(to)

    val containerOpt1 = pairOpt1.map(_._1)
    val containerOpt2 = pairOpt2.map(_._1)

    val valueOpt1 = pairOpt1.map(_._2)
    val valueOpt2 = pairOpt2.map(_._2)

    val valuePairOpt = (valueOpt1, valueOpt2).toOptionalTuple

    if (containerOpt1 != containerOpt2 ||
        valuePairOpt.exists { case (value1, value2) => 
          value1.getClass() != value2.getClass() })
      gallia.vldt._Error.Runtime.DifferingRuntimeType(from
          .toString, to.toString).throwDataError() // TODO: throw a lower level exception here    
  }
  
  // ---------------------------------------------------------------------------
  // quick-n-dirty test (TODO: port to tests):
  /*
    def main(args: Array[String]): Unit = {
      def success(from: Any, to: Any) = assert(util.Try { checkSameTypes(from, to) }.isSuccess)
      def failure(from: Any, to: Any) = assert(util.Try { checkSameTypes(from, to) }.isFailure)  
    
  		success(None, None)
      success(1, 2)
      success(Seq(1, 2), Seq(3))    
      success(Some(1), Some(2))
      success(Some(Seq(1, 2)), Some(Seq(3)))
  
      failure(1, None)
      failure(None, 1)
      
      failure(Some(1), None)
      failure(None, Some(1))    
  
      failure(Seq(1, 2), None)
      failure(None, Seq(1, 2))
  
      failure(Some(Seq(1, 2)), None)
      failure(None, Some(Seq(1, 2)))
      
      failure(     1 ,      "a")
      failure(Some(1), Some("a"))    
      
      failure(     Seq(1, 2) ,      Seq("a", "b"))
      failure(Some(Seq(1, 2)), Some(Seq("a", "b")))
  
      failure(     Seq(1, 2) , Some(Seq(1, 2)))
    }
  */

}

// ===========================================================================
