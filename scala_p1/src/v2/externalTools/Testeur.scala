package v2.externalTools

import scala.util.Random
import v2.GameState4P
import v2.SimulBot
import v2.score
import v2.stupidAgent
import v2.deadStuff.Simulateur001
import v2.bv_tronRacer
import v2.bv_zerg
import v2.bv_taker
import v2.BMap
import v2.BotVocabulary
import v2.tb006
import v2.tb005
import v2.tb003

/**
 * @author Jahan
 */
object Testeur extends App {
  
  def gameToTest(r : Long )={
    new SimulBot(0x457571FF,GameState4P.start(r,4),Array(new tb006(0x457575),new tb005(),new tb005(),new tb003)) 
    
  }
  
  
  def statsTest001{
    val r = new Random(0x8377^System.nanoTime());
    val randLong=Math.abs(r.nextLong())
    val size=4;
    
    val nbGames=1000
    val nbTurns=250
    var acc : Array[Array[Int]] = Array(Array(0,0,0,0),Array(0,0,0,0),Array(0,0,0,0),Array(0,0,0,0))
    
    def updateAcc(g : GameState4P) : List[ List[Int]]={
      var l = g.sortedResults
      
      def recU(rp : List[Tuple2[Int,Int]], rang : Int){
        if(rp.nonEmpty){
                acc(rang)(rp.head._2)=acc(rang)(rp.head._2)+1
                recU(rp.tail,rang+1)
        }
      }
      
      recU(l,0)

     
      List(
          List(acc(0)(0),acc(0)(1),acc(0)(2),acc(0)(3)),
          List(acc(1)(0),acc(1)(1),acc(1)(2),acc(1)(3)),
          List(acc(2)(0),acc(2)(1),acc(2)(2),acc(2)(3)),
          List(acc(3)(0),acc(3)(1),acc(3)(2),acc(3)(3))
          
      )
    }
    

    
    val t0 = System.nanoTime()    
    for(i<-1 until nbGames){
      
      def percentIng(l :  List[ List[Int]]) = {
        for( lvl <- l) yield{
          lvl.map { x => (x*1000)/i }
        }
      }  
      
      val gseed=r.nextLong()^System.nanoTime()
      val sim = gameToTest(gseed)    
      
      for(i<-0 until nbTurns){
        sim.turn()
      }
      
      val t1 = System.nanoTime()      
      val t : Double=(t1 - t0)           
      val currTime=(t/(1000*1000*1000))
     System.err.println(" Per sec "+(i.toDouble / currTime )+" currTime = "+currTime+"  seed="+gseed);    
      val upRes=updateAcc(sim.getState)
     System.err.println(""+i+" "+percentIng(upRes)+"  "+upRes+" "+sim.getState.sortedResults);
      
    }
    

    
  }
  
  
  def testGS001{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L,4)
    
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
    var game=GameState4P.start(64564544557L,4)
    
    val simul = new SimulBot(r.nextLong(),game,Array(new bv_tronRacer,new bv_tronRacer))
    System.err.println(""+game);
    
    for(i <- 0 until 10){
      simul.reset()
      val score = simul.eval()
      System.err.println(""+simul.getState);
      System.err.println(""+score);
    }    
  }  
  
  
 def benchGS002{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L,4)
    val simul = new SimulBot(r.nextLong(),game,Array(new bv_taker(BMap.full,0x83838L),new bv_taker(BMap.full,0x899877L)))
    
    System.err.println(""+simul.ref);
    
    def nbBoucle=120;
    
    val t0 = System.nanoTime()
    val sc=for(i <- 0 until nbBoucle) yield{
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
 
 def benchGS003{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L,4)
    val simul = new SimulBot(r.nextLong(),game,Array(new bv_taker(BMap.full,0x83838L),new bv_taker(BMap.full,0x899877L)))
    
    System.err.println(""+simul.ref);
    
    var acc=0;
    
    def nbBoucle=5000;
    
    for(i <- 0 until 10) {
      simul.reset()
     simul.turn()
    }
    
    val t0 = System.nanoTime()
    val sc=for(i <- 0 until nbBoucle) yield{
      //System.err.println(""+simul.getState);
      //System.err.println(""+score);
      
      val bv = new BotVocabulary(simul.getState)
      val sh = bv.shadows.size
      acc = acc+sh
    }    
    val t1 = System.nanoTime()
    
    val t : Double=(t1 - t0)
    
    //val sumsc=sc.foldLeft(new Tuple4[Double,Double,Double,Double](0,0,0,0))((x : Tuple4[Double,Double,Double,Double], y: score) =>new Tuple4[Double,Double,Double,Double](x._1+y.x0,x._2+y.x1,x._3+y.x2,x._4+y.x3))
    
   System.err.println(""+simul.getState +" "+acc);        
   // System.err.println(""+sumsc);
   System.err.println(" Per sec "+(nbBoucle.toDouble / (t/(1000*1000*1000)) ));

    
  }   
  
  def bench002{
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L,4)
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
    var game=GameState4P.start(64564544557L,4)
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
    // testGS002
     //benchGS002
     //bench002
     //benchGS003
     statsTest001
  }
  
}