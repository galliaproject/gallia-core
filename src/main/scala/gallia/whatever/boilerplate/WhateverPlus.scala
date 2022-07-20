package gallia
package whatever
package boilerplate

// ===========================================================================
object WhateverPlus {

  /* keep in sync with other boilerplate's Whatever{Plus,Times,...} */
  @NumberAbstraction 
  private[gallia] def apply(first: Any, second: Any): Any = {     
    first match { 
      
      // ---------------------------------------------------------------------------
      case x: Int => second match {
        case y: Int    => (x + y): Int
        case y: Double => (x + y): Double
        
        case y: Byte   => (x + y): Int
        case y: Short  => (x + y): Int
        case y: Long   => (x + y): Long

        case y: Float  => (x + y): Float

        case y: BigInt => (x + y): BigInt
        case y: BigDec => (x + y): BigDec }
      
      // ---------------------------------------------------------------------------
      case x: Double => second match {
        case y: Int    => (x + y): Double
        case y: Double => (x + y): Double
        
        case y: BigInt => (BigDec    (x) + BigDec    (y)): BigDec    
        case y: BigDec => (x + y)                        : BigDec    
        
        case y: Number => (x + y.doubleValue()): Double }

      // ---------------------------------------------------------------------------
      case x: Byte => second match {
        case y: Int    => (x + y): Int
        case y: Double => (x + y): Double
        
        case y: Byte   => (x + y): Int // see 210809113923
        case y: Short  => (x + y): Int // see 210809113923
        case y: Long   => (x + y): Long

        case y: Float  => (x + y): Float

        case y: BigInt => (x + y): BigInt
        case y: BigDec => (x + y): BigDec }
      
      // ---------------------------------------------------------------------------
      case x: Short => second match {
        case y: Int    => (x + y): Int
        case y: Double => (x + y): Double
        
        case y: Byte   => (x + y): Int // see 210809113923
        case y: Short  => (x + y): Int // see 210809113923
        case y: Long   => (x + y): Long

        case y: Float  => (x + y): Float

        case y: BigInt => (x + y): BigInt
        case y: BigDec => (x + y): BigDec }
      
      // ---------------------------------------------------------------------------
      case x: Long => second match {
        case y: Double => (x + y)             : Double
        case y: Float  => (x + y)             : Float

        case y: BigInt => (BigInt(x) + y)     : BigInt
        case y: BigDec => (x + y)             : BigDec    
        
        case y: Number => (x + y.floatValue()): Float }
      
      // ---------------------------------------------------------------------------
      case x: Float => second match {
        case y: Double => (x + y): Double

        case y: BigInt => (BigDec    (x) + BigDec    (y)): BigDec    
        case y: BigDec => (x + y)                        : BigDec    
        
        case y: Number => (x + y.floatValue()): Float }

      // ---------------------------------------------------------------------------
      case x: BigInt => second match {          
        case y: Int    => (x + y): BigInt          
        case y: Byte   => (x + y): BigInt
        case y: Short  => (x + y): BigInt
        case y: Long   => (x + y): BigInt
        case y: BigInt => (x + y): BigInt

        case y: Double => (BigDec    (x) + y): BigDec    
        case y: Float  => (BigDec    (x) + y): BigDec    
        case y: BigDec => (BigDec    (x) + y): BigDec }
      
      // ---------------------------------------------------------------------------
      case x: BigDec     => second match {
        case y: BigDec => (x + y)            : BigDec    
        case y: BigInt => (x + BigDec    (y)): BigDec    
        case y: Number => (x + y.doubleValue): BigDec }
    }
  }
}

// ===========================================================================
