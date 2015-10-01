package v2


/**
 * @author Jahan
 */
class oo002(nbPlayer: Int) extends agentAbstract {

  var currPlan: agentAbstract = null
  var ra = 0xAA88319

  var startZone: List[BMap] = null
  var rootZone: List[BMap] = null

  var myRight: BMap = null
  var myRightSz = 0

  val tron = new bv_tronRacer

  def doOnce(ref: GameState4P) {
    if (startZone == null) {
      val bv = new BotVocabulary(ref)
      startZone = bv.extractAllZone
      myRightSz = (20 * 35) / nbPlayer
      rootZone = startZone.map { x => x.noyau }

      //  System.err.println(""+startZone);

      //  System.err.println(""+scaleZone);

      // System.err.println(""+rootZone)
      myRight = rootZone.tail.minBy { x => x.closestPointHere(rootZone.head)._1 }
      // System.err.println(""+myRight)      

    }
  }
  

  def doPlan(ref: GameState4P) = {
    
    def maxB(l : List[BMap])={
      if(l.nonEmpty)
        l.maxBy { x => x.countBitset }
      else
        BMap.zero
    }
    
    val bv = new BotVocabulary(ref)
    
    def takeThere(dst : BMap)={
      val limitsToTrait = ( ~(~bv.void & ~ref.tr.pos0).scramble & dst.scramble)      
      val limits = bv.border( limitsToTrait ) & (~ref.tr.pos0)
        val res = bv.goTo(limits)
        if (res.size > 0) res(0) else tron.genMove(ref)

    }
      

    val shad = bv.shadows;
    // System.err.println("TheShadows : \n"+bv.shadows);        

    val myBiggest = maxB(shad.head)
    val themBiggest = shad.tail.map { x => if(x.nonEmpty) x.maxBy { x => x.countBitset } else BMap.zero }

    val themDangerous = themBiggest.filter { x => (x.countBitset >= (Math.max(myBiggest.countBitset,17))) }

    if (themDangerous.size > 0) {
      val maxDanger = themDangerous.maxBy { x => x.countBitset }
      //Console.err.println("TheirBig"+maxDanger);
      val res=takeThere(maxDanger)
      res
    } else {

      if (myBiggest.countBitset > 12) {
          val res=takeThere(myBiggest)
          res
      } else if (!(bv.void & myRight).isNull && (ref.tr.pos0 & myRight).isNull) {
        val res = bv.goTo(myRight)
        if (res.size > 0) res(0) else tron.genMove(ref)
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
    doOnce(ref)
    logMove.blockControl {

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