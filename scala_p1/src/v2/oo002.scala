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
    val bv = new BotVocabulary(ref)

    val shad = bv.shadows;
    // System.err.println("TheShadows : \n"+bv.shadows);        

    val myBiggest = shad.head.maxBy { x => x.countBitset }
    val themBiggest = shad.tail.map { x => x.maxBy { x => x.countBitset } }

    val themDangerous = themBiggest.filter { x => (x.countBitset >= (Math.max(myBiggest.countBitset,9))) }

    if (themDangerous.size > 0) {
      val maxDanger = themDangerous.maxBy { x => x.countBitset }
      //Console.err.println("TheirBig"+maxDanger);
      val res = bv.goTo(maxDanger)
      if (res.size > 0) res(0) else tron.genMove(ref)
    } else {

      if (myBiggest.countBitset > 9) {
        val limits = bv.border((myBiggest.scramble | ref.tr.pos0) & (~ref.tr.pos0))
       // Console.err.println("MyBig"+myBiggest+" limit "+limits);
        val res = bv.goTo(limits)
        if (res.size > 0) res(0) else tron.genMove(ref)
      } else if (!(bv.void & myRight).isNull && (ref.tr.pos0 & myRight).isNull) {
        val res = bv.goTo(myRight)
        if (res.size > 0) res(0) else tron.genMove(ref)
      } else {
        tron.genMove(ref)
      }
    }

  }

  var logMove: log = new log
  override def backMove() {
    //System.err.println("backing "+logMove);
    logMove.undo()
  }

  def genMove(ref: GameState4P) = {
    doOnce(ref)
    logMove.blockControl {
      doPlan(ref)

    }

  }

}