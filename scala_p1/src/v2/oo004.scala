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
  
  def forseeMovesSimple(to: BMap,ref: GameState4P) = {
    val zerg = new bv_followTrail(to)(identity)
    val sim = new SimulBot(0, ref, Array(zerg, zerg, new bv_doNothing, new bv_doNothing))
    var dir = 0

    while (GameState4P.m(dir)(0) != 4) {
      dir = sim.turn()
    }
    
    val success = ((sim.getState.tr.pos0 & to)^to).isNull
   // System.err.println("\n"+sim.getState);
    
   // val sim2 = new SimulBot(0, sim.getState, Array(new bv_taker(BMap.full,0xFF92888), new bv_taker(BMap.full,0xFF89398), new bv_doNothing, new bv_doNothing))

   // for(i <- 0 until 100){
   //   dir = sim2.turn()
   // }
    
    if(success) sim.getState.myRelScore else 0
  }

  def doPlan(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)        
     val targ = bv.firstTronZoneHeuristic
     val tr = bv.firstZoneHeuristic
     
     val captSt : List[BMap]=bv.basicCapturePathTry
     
    val tmpF0 = for (d <- 0 until 4; s <- List(4,10,20)) yield {
      (new Tuple2(d, s))
    }
    
    def genBmSquare(p: Seq[Tuple2[Int, Int]], bv: BotVocabulary) = {
      for (Tuple2(x, y) <- p) yield {
        bv.border(bv.squareInDir(x, y) & bv.st.tr.void)
      }
    }    
    
    val bmSqToTry = genBmSquare(tmpF0, bv)
     
     val currS=ref.myRelScore     
     //System.err.println(""+captSt)
     
   //  val scoresBasicManeu=captSt.map { x => forseeMovesSimple(x, ref) - currS }
     
     val ll=captSt
     val ol=bmSqToTry.toList
    
    val basicEval= (ll:::ol).filter { x => bv.is_capturing(x) }.map { x => (forseeMovesSimple(x, ref) - currS,x) }
    
    //System.err.println("scores "+scoresBasicManeu);
    
    val maxEval=if(basicEval.size>0) basicEval.maxBy(x => x._1) else (0,BMap.zero)
    
    if(maxEval._1 > 20){
      
      
      System.err.println("Follow evaluation"+maxEval)
      
      val zerg = new bv_followTrail(maxEval._2)(identity)
      val to=zerg.genMove(ref)
      to
    }else{
          tron.genMove(ref)
    }
    
    //System.err.println("Best plan : "+maxEval);
     
    // System.err.println("tron"+targ);
    //System.err.println("first"+tr);
    // System.err.println("dist "+targ._3);
    // System.err.println("tron + first "+((targ._2 & tr) | ref.pos.pos0));
    
    

        
        

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