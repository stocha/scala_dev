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
    
    val simul = new Simulateur001(r.nextLong(),game)
    System.err.println(""+game);
    
    for(i <- 0 until 80){
      simul.turn()
      game= simul.getState
      System.err.println("Turn "+i);
      System.err.println(""+game);
    }    
  }
  
  def bench001{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L)
    val simul = new Simulateur001(r.nextLong(),game)
    
    def nbBoucle=50000;
    
    val t0 = System.nanoTime()
    for(i <- 0 until nbBoucle){
      if(i%700==0) simul.reset()
      simul.turn()
    }       
    val t1 = System.nanoTime()
    
    val t : Double=(t1 - t0)
    
   System.err.println(""+simul.getState);        
   System.err.println(" Per sec "+(nbBoucle.toDouble / (t/(1000*1000*1000)) ));

    
  }
  
   override def main(args: Array[String]) {
    //testGS001
     bench001
  }
  
}