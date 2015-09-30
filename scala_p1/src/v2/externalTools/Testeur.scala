package v2.externalTools

import scala.util.Random
import v2.GameState4P
import v2.SimulBot
import v2.score
import v2.stupidAgent
import v2.deadStuff.Simulateur001

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
  
  def testGS002{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L)
    
    val simul = new SimulBot(r.nextLong(),game,Array(new stupidAgent))
    System.err.println(""+game);
    
    for(i <- 0 until 10){
      simul.reset()
      val score = simul.eval()
      System.err.println(""+simul.getState);
      System.err.println(""+score);
    }    
  }  
  
  def bench002{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L)
    val simul1 = new Simulateur001(r.nextLong(),game)
    
    for(i<-0 until 4){
      simul1.turn() 
    }
    
    val simul = new Simulateur001(r.nextLong(),simul1.getState)
    
    System.err.println(""+simul.ref);
    
    def nbBoucle=100;
    
    val t0 = System.nanoTime()
    val sc=  for(i <- 0 until nbBoucle) yield{
      simul.reset()
      val score = simul.eval()
      //System.err.println(""+simul.getState);
      //System.err.println(""+score);
      score
    }    
    val t1 = System.nanoTime()
    
    val t : Double=(t1 - t0)
    
    val sumsc=sc.foldLeft(new Tuple4[Double,Double,Double,Double](0,0,0,0))((x : Tuple4[Double,Double,Double,Double], y: score) =>new Tuple4[Double,Double,Double,Double](x._1+y.x0,x._2+y.x1,x._3+y.x2,x._4+y.x3))
    
   System.err.println(""+simul.getState);        
    System.err.println(""+sumsc);
   System.err.println(" Per sec "+(nbBoucle.toDouble / (t/(1000*1000*1000)) ));

    
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
     testGS002
     //bench002
  }
  
}