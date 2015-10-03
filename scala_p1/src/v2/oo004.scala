package v2

/**
 * @author Jahan
 */
class oo004 extends agentAbstract {

  var currPlan: agentAbstract = null
  var ra = 0xAA88319

  val tron = new agentAbstract {

    def genMove(ref: GameState4P) = {

      val bv = new BotVocabulary(ref)
      
      def anyOpen = {
        if (bv.void.isNull) {
          4
        } else {
    
           val targArea = bv.border(bv.void.split.maxBy { x => x.countBitset })
            val resp = bv.goTo(targArea)
    
            if (resp.nonEmpty) {
              resp(0)
            } else 4       
        }        
        
      }

      def gosomeOpenBorder = {

        val targ = bv.firstTronZoneHeuristic
        //Console.err.println("raw front\n"+targ)
        val targSS = bv.border(targ._1.closeDiag)
        val targf = if (targSS.isNull) targ._2.closeDiag else targSS
       //   Console.err.println("f front\n"+targf)
        // val targfsp = targf.split
        //if (targfsp.nonEmpty) {
        //val mtarg = targfsp.maxBy { x => x.countBitset }
        val mtarg = targf
        val resp = bv.goToWithVoid(mtarg)
        ra = ((ra << 3) + 13) & 0xFFFFFF;
        def rind = ((ra % resp.size) & 3)
        if (resp.size > 0 ) {
          resp(rind)
        } else {
          anyOpen
        }
      }

      if ((ref.pos.pos0 & ref.tr.pos0).isNull) {
        //  System.err.println("FUCK " +ref.myRelScore+" Scores "+ref.scores)
        if (ref.myRelScore > 0) 4 else gosomeOpenBorder
      } else {
        gosomeOpenBorder
      }
    }
  }

  def takeThere(b: BMap, ref: GameState4P) = {
    // System.err.println("Taking : "+b)

    currPlan = new bv_taker(b, 0x4157457)
    currPlan.genMove(ref)

  }

  def forseeMovesSimple(to: BMap, ref: GameState4P) = {
    val bv = new BotVocabulary(ref)
    val capt = bv.what_capturing(to)
    val ag = new bv_followTrail(to)(identity)
    val zerg = new bv_followTrail(capt)(identity)
    val sim = new SimulBot(0, ref, Array(ag, zerg, new bv_doNothing, new bv_doNothing))
    var dir = 0

    while (GameState4P.m(dir)(0) != 4) {
      dir = sim.turn()
    }

    val success = ((sim.getState.tr.pos0 & capt) ^ capt).isNull
    // System.err.println("\n"+sim.getState);

    // val sim2 = new SimulBot(0, sim.getState, Array(new bv_taker(BMap.full,0xFF92888), new bv_taker(BMap.full,0xFF89398), new bv_doNothing, new bv_doNothing))

    // for(i <- 0 until 100){
    //   dir = sim2.turn()
    // }

    if (success) {

      //   System.err.println("success \n"+  to+" \n "+capt);
      capt.countBitset

    } else 0
  }

  def doPlan(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)
    val targ = bv.firstTronZoneHeuristic
    val tr = bv.firstZoneHeuristic

    val captSt: List[BMap] = bv.basicCapturePathTry

    val tmpF0 = for (d <- 0 until 4; s <- List(4, 10, 20)) yield {
      (new Tuple2(d, s))
    }

    def genBmSquare(p: Seq[Tuple2[Int, Int]], bv: BotVocabulary) = {
      for (Tuple2(x, y) <- p) yield {
        bv.border(bv.squareInDir(x, y) & bv.st.tr.void)
      }
    }

    val bmSqToTry = genBmSquare(tmpF0, bv)

    val currS = ref.myRelScore
    //System.err.println(""+captSt)

    //  val scoresBasicManeu=captSt.map { x => forseeMovesSimple(x, ref) - currS }

    val ll = captSt
    val basicEval = (ll).filter { x => bv.is_capturing(x) }.map { x => (forseeMovesSimple(x, ref) - currS, x) }
    val maxEval = if (basicEval.size > 0) basicEval.maxBy(x => x._1) else (0, BMap.zero)

    //System.err.println("scores "+scoresBasicManeu);
    val MinValForPlan = 3
    if (maxEval._1 > MinValForPlan) {

      val ll = captSt
      val basicEval = (ll).filter { x => bv.is_capturing(x) }.map { x => (forseeMovesSimple(x, ref) - currS, x) }
      val maxEval = if (basicEval.size > 0) basicEval.maxBy(x => x._1) else (0, BMap.zero)
      if (maxEval._1 > MinValForPlan) {
        //   System.err.println("Follow evaluation" + maxEval)

        val zerg = new bv_followTrail(maxEval._2)(identity)
        val to = zerg.genMove(ref)
        to
      } else {
        tron.genMove(ref)
      }

      //  System.err.println("Follow evaluation" + maxEval)

      val zerg = new bv_followTrail(maxEval._2)(identity)
      val to = zerg.genMove(ref)
      to
    } else {
     // Console.err.println("Tron go");
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

      if (currPlan == null) {
        doPlan(ref)
      } else {
        val m = currPlan.genMove(ref)
        if (m != 4) m else {
          currPlan = null
          doPlan(ref)

        }
      }

    }

  }

}