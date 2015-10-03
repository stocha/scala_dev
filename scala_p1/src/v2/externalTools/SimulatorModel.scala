package v2.externalTools

import scala.util.Random
import v2.BMap
import v2.GameState4P
import v2.SimulBot
import v2.botVocTest
import v2.bv_taker
import v2.bv_tronRacer
import v2.stupidAgent
import v2.tb001
import v2.tb003
import v2.test_bv_square
import v2.test_bv_squareUndo
import v2.oo004
import v2.oo005
import v2.oo002

/**
 * @author Jahan
 */
object SimulatorModel {
  def getModel = {
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    val size=2;
    var game=GameState4P.start(64564544557L,size)
    
    //val simul = new SimulBot(r.nextLong(),game,Array(new stupidAgent,new stupidAgent,new stupidAgent,new stupidAgent))
    
    val simul = new SimulBot(r.nextLong(),game,Array(new stupidAgent,new botVocTest,new test_bv_square(2,7,false),new test_bv_squareUndo(2,7,true)))
    
    val tb001T = new SimulBot(r.nextLong(),game,Array(new stupidAgent,new tb001,new test_bv_square(2,7,false),new test_bv_squareUndo(2,7,true)))
    
    val tb002T = new SimulBot(r.nextLong(),game,Array(new tb003,new tb003,new test_bv_square(2,7,false),new test_bv_squareUndo(2,7,true)))
   // val tb003T = new SimulBot(r.nextLong(),game,Array(new stupidAgent,new stupidAgent,new stupidAgent,new tb003))
    
     val tb003T = new SimulBot(r.nextLong(),game,Array(new tb003,new tb003,new tb003,new tb003))
    
   //  val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,2),Array(new oo001,new tb003))
    //val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,2),Array(new oo001,new bv_racer))
    //val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,2),Array(new oo001,new bv_tronFrontierInside))
   // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,2),Array(new bv_tronRacer,new bv_racer))
   // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,2),Array(new stupidAgent,new bv_tronRacer))
  //  val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,2),Array(new bv_tronRacer,new tb001))
   // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,2),Array(new bv_tronRacer,new tb003))
   // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new oo002(size),new tb003,new tb003,new tb003))
   // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new oo002(size),new tb003))
   //  val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new oo002(size),new oo002(size)))
    //val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new tb003,new oo003,new tb003,new tb003))
    
   // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new oo003,new oo003,new tb003,new tb003))
    //val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new tb003,new tb004,new tb003,new tb004))
    
   // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new tb003,new tb005,new tb003,new tb003))
    
    
    val bot =new bv_taker(BMap.full,0xF44824F75L*4745)
    //val oo001T = new SimulBot(r.nextLong(),GameState4P.start(0x645657FL,size),Array(bot,bot,bot,bot))
   // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(bot,new oo004))
     //val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new oo004,new oo004))
    // val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new tb003,new oo004))
    //val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new bv_tronRacer,new oo004))
     val oo001T = new SimulBot(r.nextLong(),GameState4P.start(64564544557L,size),Array(new oo005,new oo002(size)))
    
    //simul
    oo001T
  }
}