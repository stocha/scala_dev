package v2

import scala.util.Random

/**
 * @author Jahan
 */
object Testeur extends App {
  
  
  def testGS001{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L)
    
    System.err.println(""+game);
    
    for(i <- 0 until 10){
      val n = r.nextInt(GameState4P.moveMax)
      game= game.transition(n)
      System.err.println("Turn "+i);
      System.err.println(""+game);
    }    
  }
  
  def bench001{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L)
    
    def nbBoucle=1000000;
    
    val t0 = System.nanoTime()
    for(i <- 0 until nbBoucle){
      val n = r.nextInt(GameState4P.moveMax)
      game= game.transition(n)

    }       
    val t1 = System.nanoTime()
    
    val t : Double=(t1 - t0)
    
   System.err.println(""+game);        
   System.err.println(" Per sec "+(nbBoucle.toDouble / (t/(1000*1000*1000)) ));

    
  }
  
   override def main(args: Array[String]) {
    //testGS001
     bench001
  }
  
}