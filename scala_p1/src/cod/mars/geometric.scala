package cod.mars

/**
 * @author Jahan
 */
object geometric extends App {

  type StructCoord = { def getCoord(): point }

  object point {
    def apply(dx: Double, dy: Double) = {
      new point(dx, dy)
    }
    
    implicit def richPoint(x : StructCoord){
      new pointManip(x)
    }
  }

  class point(val x: Double, val y: Double) {

  }

  trait hasCoord {
    def getCoord: point
  }

  class pointManip(val p : StructCoord) {

  }
  
  
   override def main(args: Array[String]) {
     Console.err.println(" Hello there");
   
   }
}