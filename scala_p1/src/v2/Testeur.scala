package v2

import scala.util.Random

/**
 * @author Jahan
 */
object Testeur extends App {
  
  
  def testGS001{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    val game=GameState4P.start(64564544557L)
    
    System.err.println(""+game);
    
  }
  
   override def main(args: Array[String]) {
    testGS001
  }
  
}