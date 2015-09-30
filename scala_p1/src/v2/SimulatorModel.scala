package v2

import scala.util.Random

/**
 * @author Jahan
 */
object SimulatorModel {
  def getModel = {
    val r = new Random(0x8377);
    val randLong=Math.abs(r.nextLong())
    var game=GameState4P.start(64564544557L)
    
    //val simul = new SimulBot(r.nextLong(),game,Array(new stupidAgent,new stupidAgent,new stupidAgent,new stupidAgent))
    
    val simul = new SimulBot(r.nextLong(),game,Array(new stupidAgent,new botVocTest,new test_bv_square(2,7,false),new test_bv_squareUndo(2,7,true)))
    
    val tb001T = new SimulBot(r.nextLong(),game,Array(new stupidAgent,new tb001,new test_bv_square(2,7,false),new test_bv_squareUndo(2,7,true)))
    
    
    //simul
    tb001T
  }
}