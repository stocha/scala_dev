package v2

/**
 * @author Jahan
 */
class oo001 extends agentAbstract {

  var currPlan: agentAbstract = null
    var ra=0xAA88319

  val stopr: ((GameState4P, BMap) => Boolean) = { (x, y) => !((y & (x.tr.void | x.pos.pos0)) == y) }

  def plansTrailTry(p: Array[BMap]) = {
    val sorted = p

    sorted.map { pp => new Tuple2(pp, new bv_trailStop(pp)(stopr)(x => x)) }

  }

  def tryPlansList(p: List[Tuple2[BMap, agentAbstract]], bv: BotVocabulary): Int = {
    if (p.isEmpty) -1 else {
      bv.forsee_with(new bv_zerg(p.head._1), p.head._2)((x: GameState4P) => ((p.head._1 & x.tr.pos0) == p.head._1))(p.size)((x: GameState4P) => false)(tryPlansList(p.tail, bv))
    }

  }

  def genBmSquare(p: Seq[Tuple2[Int, Int]], bv: BotVocabulary) = {
    for (Tuple2(x, y) <- p) yield {
      bv.border(bv.squareInDir(x, y) & bv.st.tr.void)
    }
  }
  

  def doTronFirst(ref: GameState4P) ={ 
    
    val bv = new BotVocabulary(ref)
    val targraw= bv.firstTronZoneHeuristic 
    val targSplit= BMap.closeDiag(targraw, bv.void).split
    //Console.err.println("raw front\n"+targraw)
    
    if(targSplit.size>0){
      
      val mtarg=targSplit.maxBy { x => x.countBitset }
          
        val targf= bv.border(mtarg)
        //Console.err.println("f front\n"+targf)
        
    
        if(targf.countBitset > 1){
          currPlan=null
          val resp = bv.goToWithVoid(targf)
          ra=((ra<<3)+13)&0xFFFFFF;
          ra=(ra*ra) / 7 & 0x98293 + ra
          Console.err.println(""+ra+" "+resp)
          def rind= (((ra>>2) % resp.size) & 3)
          if (resp.size > 0){
            resp(rind)
          }  else {
              4
          }      
          
        }else{
          doPlan(ref)
        }       
        
        
    }else{
      doPlan(ref)
    }
  }
  
  def doPlan(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)

    val specialVoid = ref.tr.void | ref.tr.pos0
    //Console.err.println("specialVoid\n"+specialVoid)
    val targNoBorder = bv.border(ref.tr.void) & ~BMap.border
    val targ = if (targNoBorder.isNull) (BMap.border & ref.tr.void) else targNoBorder
    //Console.err.println("frontier\n"+targ)
    val resp = bv.goTo(targ)

    if (resp.size > 0) resp(0) else {
      //Console.err.println("Nowhere to go !\n");
      val lastChance = bv.goTo(ref.tr.void)

      if (lastChance.size > 0) {
        lastChance(0)
      } else
        4

    }

  }

  var logMove: log = new log
  override def backMove() {
    //System.err.println("backing "+logMove);
    logMove.undo()
  }

  def genMove(ref: GameState4P) = {
    logMove.blockControl {
      doTronFirst(ref)


    }

  }

}