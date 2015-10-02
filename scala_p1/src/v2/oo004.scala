package v2

/**
 * @author Jahan
 */
class oo004 extends agentAbstract {

  var currPlan: agentAbstract = null
  var ra = 0xAA88319

  val tron = new bv_tronRacer
  
  def takeThere(b : BMap,ref: GameState4P)={
   // System.err.println("Taking : "+b)
    
    currPlan=new bv_taker(b,0x4157457)
    currPlan.genMove(ref)
    
  }
  


  def doPlan(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)        
     val targ = bv.firstTronZoneHeuristic
     val tr = bv.firstZoneHeuristic
     
     val captSt=bv.dirCaptureStraight
     
     System.err.println(""+captSt)
     
    // System.err.println("tron"+targ);
    //System.err.println("first"+tr);
    // System.err.println("dist "+targ._3);
    // System.err.println("tron + first "+((targ._2 & tr) | ref.pos.pos0));
    
    
    tron.genMove(ref)
        
        

    //4
  }

  var logMove: log = new log
  override def backMove() {
    //System.err.println("backing "+logMove);
    logMove.undo()
  }

  def genMove(ref: GameState4P) = {
    logMove.blockControl {
     // currPlan=null

          if(currPlan==null){
           doPlan(ref )
          }else{
            val m = currPlan.genMove(ref)
            if(m!=4) m else{
              currPlan=null
              doPlan(ref)
              
            }
          }

    }

  }

}