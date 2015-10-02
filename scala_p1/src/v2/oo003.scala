package v2

/**
 * @author Jahan
 */
class oo003 extends agentAbstract {

  var currPlan: agentAbstract = null
  var ra = 0xAA88319

  val tron = new bv_tronRacer
  
  def takeThere(b : BMap,ref: GameState4P)={
   // System.err.println("Taking : "+b)
    
    currPlan=new bv_taker(b,0x4157457)
    currPlan.genMove(ref)
    
  }
  


  def doPlan(ref: GameState4P) = {
    
    def maxB(l : List[BMap])={
      if(l.nonEmpty)
        l.maxBy { x => x.countBitset }
      else
        BMap.zero
    }
    
    val bv = new BotVocabulary(ref)
    val shad = bv.shadows;
    // System.err.println("TheShadows : \n"+bv.shadows);        

    val myBiggest = maxB(shad.head)
    val themBiggest = shad.tail.map { x => if(x.nonEmpty) x.maxBy { x => x.countBitset } else BMap.zero }

    val themDangerous = themBiggest.filter { x => (x.countBitset >= 60) }

    if (themDangerous.size > 0) {
      val maxDanger = themDangerous.maxBy { x => x.countBitset }
      //Console.err.println("TheirBig"+maxDanger);
      val res=takeThere(maxDanger,ref)
      res
    } else {

      if (myBiggest.countBitset > 50) {
          val res=takeThere(myBiggest,ref)
          res
      } else { 
        tron.genMove(ref)
      }
    }
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