package v2

import scala.util.Random

/**
 * @author Jahan
 * 
 * 
 *     new SimulBot(0x457571FF,GameState4P.start(r,4),Array(new tb006(0x457575),new tb005(),new tb005(),new tb003)) 
 * 
 * ratioEmpty 80
 * 157 List(List(146, 203, 356, 292), List(292, 261, 210, 235), List(363, 184, 178, 273), List(197, 350, 254, 197))  List(List(23, 32, 56, 46), List(46, 41, 33, 37), List(57, 29, 28, 43), List(31, 55, 40, 31)) List((217,2), (206,3), (158,0), (119,1))
 * 
 * 
 * ratioEmpty 95
 * 157 List(List(197, 229, 273, 299), List(248, 242, 292, 216), List(388, 210, 165, 235), List(165, 318, 267, 248))  List(List(31, 36, 43, 47), List(39, 38, 46, 34), List(61, 33, 26, 37), List(26, 50, 42, 39)) List((213,3), (205,2), (154,0), (128,1))
 * 
 * 
 */
class tb006(seed: Long) extends agentAbstract {

  var currPlan: agentAbstract = null
  var ra = new Random(0x883789 ^ seed)
  val MinValForPlan = 3

  val ratioEmpty = 95

  val cyclebreaker = Array(0, 1, 2, 3)

  def insertInCycleBreak(i: Int, bv: BotVocabulary): Int = {
    cyclebreaker(3) = cyclebreaker(2)
    cyclebreaker(2) = cyclebreaker(1)
    cyclebreaker(1) = cyclebreaker(0)
    cyclebreaker(0) = i

    def eq(i: Int, j: Int) = {
      cyclebreaker(i) == cyclebreaker(j)
    }

    def opeq(i: Int, j: Int) = {
      cyclebreaker(i) == ((cyclebreaker(j) + 2) % 4)
    }

    if (eq(0, 2) && eq(3, 1) && opeq(1, 2)) {
      val d = bv.goTo(~bv.me)
      val r = ra.nextInt(d.size)
      d(r)
    } else i

  }

  def myGoElse(bv: BotVocabulary, dest: BMap)(elsedo: => Int) = {
    val r = bv.greedyGoto(dest)
    // System.err.println(""+dest);
    val dir = if (r.nonEmpty) r(ra.nextInt(r.size)) else elsedo
    insertInCycleBreak(dir, bv)

  }

  val tron = new agentAbstract {
    def genMove(ref: GameState4P) = {
      val bv = new BotVocabulary(ref)

      def anyOpen = {
        if (bv.void.isNull) {
          4
        } else {

          val targArea = bv.border(bv.void.split.maxBy { x => x.countBitset })
          val resp = myGoElse(bv, targArea) { 4 }
          resp
        }

      }

      def gosomeOpenBorder = {

        val targ = bv.firstTronZoneHeuristic

        //Console.err.println("raw front\n"+targ)
        val targSS = bv.border(targ._1.closeDiag)
        val targf = if (targSS.isNull) targ._2.closeDiag else targSS
        //  Console.err.println("f front\n"+targf)
        // val targfsp = targf.split
        //if (targfsp.nonEmpty) {
        //val mtarg = targfsp.maxBy { x => x.countBitset }
        val mtarg = targf
        val resp = myGoElse(bv, mtarg) { anyOpen }
        resp
      }
      if (((ref.pos.pos0 & ref.tr.pos0) | ref.tr.void).isNull) {
        //System.err.println("Interlock " + ref.myRelScore + " Scores " + ref.scores)
        if (ref.myRelScore > 0) 4 else gosomeOpenBorder
      } else {
        gosomeOpenBorder
      }
    }

  }

  /*def takeThere(b: BMap, ref: GameState4P) = {
    // System.err.println("Taking : "+b)        

    currPlan = new bv_taker(b, 0x4157457)
    currPlan.genMove(ref)

  }*/

  def forseeCaptureZergedFully(to: BMap, ref: GameState4P) = {
    val bv = new BotVocabulary(ref)
    val capt = bv.what_capturing(to)
    val ag = new bv_followTrail(to)(identity)
    val zerg1 = new bv_followTrail(capt)(identity)
    val zerg2 = new bv_followTrail(capt)(identity)
    val zerg3 = new bv_followTrail(capt)(identity)
    val sim = new SimulBot(0, ref, Array(ag, zerg1, zerg2, zerg3))
    var dir = 0

    //System.err.println("Simuling try "+(ref.tr.pos0 | to));

    while (GameState4P.m(dir)(0) != 4) {
      dir = sim.turn()
    }

    val success = ((sim.getState.tr.pos0 & capt) ^ capt).isNull
    //     System.err.println("\n"+sim.getState);

    // val sim2 = new SimulBot(0, sim.getState, Array(new bv_taker(BMap.full,0xFF92888), new bv_taker(BMap.full,0xFF89398), new bv_doNothing, new bv_doNothing))

    // for(i <- 0 until 100){
    //   dir = sim2.turn()
    // }

    if (success) {

      //   System.err.println("success \n"+  to+" \n "+capt);
      capt.countBitset

    } else 0
  }

  def forseeCaptureZergedBy(to: BMap, ref: GameState4P, play: Int) = {
    val bv = new BotVocabulary(ref)
    val capt = bv.what_capturing(to)
    val ag = new bv_followTrail(to)(identity)
    val zerg1 = if (play == 1) new bv_followTrail(capt)(identity) else new bv_doNothing
    val zerg2 = if (play == 2) new bv_followTrail(capt)(identity) else new bv_doNothing
    val zerg3 = if (play == 3) new bv_followTrail(capt)(identity) else new bv_doNothing
    val sim = new SimulBot(0, ref, Array(ag, zerg1, zerg2, zerg3))
    var dir = 0

    //System.err.println("Simuling try "+(ref.tr.pos0 | to));

    while (GameState4P.m(dir)(0) != 4) {
      dir = sim.turn()
    }

    val success = ((sim.getState.tr.pos0 & capt) ^ capt).isNull
    //     System.err.println("\n"+sim.getState);

    // val sim2 = new SimulBot(0, sim.getState, Array(new bv_taker(BMap.full,0xFF92888), new bv_taker(BMap.full,0xFF89398), new bv_doNothing, new bv_doNothing))

    // for(i <- 0 until 100){
    //   dir = sim2.turn()
    // }

    if (success) {

      //   System.err.println("success \n"+  to+" \n "+capt);
      capt.countBitset

    } else 0
  }

  def forseeConcurrentCaptures(mine: BMap, theirs: BMap, ref: GameState4P, play: Int) = {
    val oldsc = ref.myRelScore

    val bv = new BotVocabulary(ref)
    val captmine = bv.what_capturing(mine)
    val capttheir = bv.what_capturing(theirs)
    val ag = new bv_followTrail(mine | capttheir)(identity)
    val capt = theirs | captmine
    val zerg1 = if (play == 1) new bv_followTrail(capt)(identity) else new bv_doNothing
    val zerg2 = if (play == 2) new bv_followTrail(capt)(identity) else new bv_doNothing
    val zerg3 = if (play == 3) new bv_followTrail(capt)(identity) else new bv_doNothing
    val sim = new SimulBot(0, ref, Array(ag, zerg1, zerg2, zerg3))
    var dir = 0

    //System.err.println("Simuling try "+(ref.tr.pos0 | to));

    while (GameState4P.m(dir)(0) != 4) {
      dir = sim.turn()
    }

    //     System.err.println("\n"+sim.getState);

    // val sim2 = new SimulBot(0, sim.getState, Array(new bv_taker(BMap.full,0xFF92888), new bv_taker(BMap.full,0xFF89398), new bv_doNothing, new bv_doNothing))

    // for(i <- 0 until 100){
    //   dir = sim2.turn()
    // }

    (sim.getState.myRelScore - oldsc)
  }

  def eCaptureLines(other: GameState4P, play: Int) = {
    val ref = other.swap(play)
    val bv = new BotVocabulary(ref)

    val captSt: List[BMap] = bv.basicCapturePathTry
    val basicEval = (captSt).filter { x =>
      {
        bv.is_capturing(x) && (forseeCaptureZergedBy(x, ref, play) == 0)
      }
    }

    if (basicEval.isEmpty) (BMap.zero, BMap.zero) else {
      val res = basicEval.maxBy { x => bv.what_capturing(x).countBitset }

      //   System.err.println("==>"+res)

      (res, bv.what_capturing(res))
    }
  }

  def doPlan_NonDouble(ref: GameState4P, captSt: List[BMap]) = {
    val bv = new BotVocabulary(ref)
    val currS = ref.myRelScore
    val ll = captSt
    val basicEval = (ll).filter { x => bv.is_capturing(x) }.map { x => (forseeCaptureZergedFully(x, ref) - currS, x) }
    val maxEval = if (basicEval.size > 0) basicEval.maxBy(x => x._1) else (0, BMap.zero)
    if (maxEval._1 > MinValForPlan) {
      //   System.err.println("Follow evaluation" + maxEval)

      val zerg = new bv_followTrail(maxEval._2)(identity)
      val to = zerg.genMove(ref)
      
     // System.err.println("Capturing  "+maxEval._2)
      to
    } else {
      tron.genMove(ref)
    }

    //  System.err.println("Follow evaluation" + maxEval)

    val zerg = new bv_followTrail(maxEval._2)(identity)
    val to = zerg.genMove(ref)
    to
  }

  def doTakeEmpty(ref: GameState4P) = {

    if (ref.tr.void.countBitset < ratioEmpty * (GameState4P.WH) / 100) {
      // System.err.println("Empty phase");

      val bv = new BotVocabulary(ref)
      val concZone = bv.firstZoneHeuristic & ref.tr.pos0
      val accessVoid = BMap.followTrail(concZone, ref.tr.void)

      if (accessVoid.isNull) {
        tron.genMove(ref)
      } else {

        //System.err.println("accessVoid "+accessVoid)
        val allTargs = accessVoid.split.map { x => val b = bv.border(x); val e = BMap.enclosed(bv.me | b, bv.void); (b, e.countBitset) }
        if (allTargs.nonEmpty) {
          val t = allTargs.maxBy(x => x._2)._1
          myGoElse(bv, t) { tron.genMove(ref) }

        } else {
          val allTargs = ref.tr.void.split.map { x => val b = bv.border(x); val e = BMap.enclosed(bv.me | b, bv.void); (b, e.countBitset) }
          val t = allTargs.maxBy(x => x._2)._1
          myGoElse(bv, t) { tron.genMove(ref) }
        }

      }

    } else {
      // System.err.println("Border phase");
      tron.genMove(ref)
    }

  }

  def doPlan(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)
    val currS = ref.myRelScore
    val targ = bv.firstTronZoneHeuristic
    val tr = bv.firstZoneHeuristic

    val captSt: List[BMap] = bv.basicCapturePathTry.filter { x => x.notNull }

    val tmpF0 = for (d <- 0 until 4; s <- List(4, 10, 20)) yield {
      (new Tuple2(d, s))
    }

    def genBmSquare(p: Seq[Tuple2[Int, Int]], bv: BotVocabulary) = {
      for (Tuple2(x, y) <- p) yield {
        bv.border(bv.squareInDir(x, y) & bv.st.tr.void)
      }
    }

    val bmSqToTry = genBmSquare(tmpF0, bv)

    //System.err.println(""+captSt)

    //  val scoresBasicManeu=captSt.map { x => forseeMovesSimple(x, ref) - currS }

    val ll = captSt
    val basicEval = (ll).filter { x => bv.is_capturing(x) }.map { x => (forseeCaptureZergedFully(x, ref) - currS, x) }
    // val basicEval = List[Tuple2[Int,BMap]]()
    val maxEval = if (basicEval.size > 0) basicEval.maxBy(x => x._1) else (0, BMap.zero)

    //System.err.println("scores "+scoresBasicManeu);

    def doThemEnemy(l: List[Int]): Int = {

      if (l.isEmpty) {

        if (maxEval._1 > MinValForPlan) {
          doPlan_NonDouble(ref, captSt)
        } else {
          doTakeEmpty(ref)

        }
      } else {
        val idE = l.head
        val enemyCapturePath = eCaptureLines(ref, idE)
        if (enemyCapturePath._1.notNull) {
          //Attention !
          if (maxEval._1 <= MinValForPlan) {
            val toDef = bv.goTo(enemyCapturePath._2)
            if (toDef.nonEmpty) toDef(0) else tron.genMove(ref)
          } else {
            val futurBothList = captSt.map { x => (forseeConcurrentCaptures(x, enemyCapturePath._1, ref, idE), x) }
            val workingOnes = futurBothList.filter { x => x._1 > currS }
            if (workingOnes.nonEmpty) {
              val bmTarg = workingOnes.maxBy { x => x._1 }._2
              val toNinja = bv.goTo(bmTarg)
              //  System.err.println("Conflicting capture, ninja going ");
              if (toNinja.nonEmpty) toNinja(0) else tron.genMove(ref)
            } else {
              // System.err.println("Conflicting capture, aborting ");
              val toDef = bv.goTo(enemyCapturePath._2)
              if (toDef.nonEmpty) toDef(0) else tron.genMove(ref)
            }
          }
        } else {
          doThemEnemy(l.tail)

        }
      }
    }
    
    val ordPlay= ref.sortedResults
    def expSup (l : List[Tuple2[Int,Int]]) : List[Int] = {
      if(l.isEmpty) List() else{
        if(l.head._2==0){
          l.tail.map { x => x._2 }
        }else{
          expSup(l.tail)
        }
        
      }
      
    }
    val listErb= expSup(ref.sortedResults);
    //System.err.println("classement infs: "+listErb)
    
    val listE = if(listErb.nonEmpty) listErb else {
      val minE = ref.sortedResults.filter(p => p._2 != 0).minBy(p => p._1)._2
      
      List(minE)
    }
    
   // System.err.println("Considered e "+listE);
    //val listE = List(1, 2, 3)    
    doThemEnemy(listE)

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