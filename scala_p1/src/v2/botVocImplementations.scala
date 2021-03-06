package v2

/**
 * @author Jahan
 */

class log {
  private var donel: List[Int] = List()
  private var undol: List[Int] = List()
  private var t = 0

  override def toString = {
    var res = ""
    res = res + t + "  " + donel + "   /    " + undol

    res
  }

  private def stack(v: Int) {
    donel = v :: donel
    t = t + 1
  }

  private def canReplay = {
    !undol.isEmpty
  }

  def hasStarted = {
    t >= 0
  }

  def undo() {
    if (t > 0) {

      undol = donel.head :: undol
      donel = donel.tail
    }
    t = t - 1
  }

  private def popRedo() = {
    t = t + 1
    val r = undol.head;
    donel = undol.head :: donel
    undol = undol.tail
    r
  }

  private def inc() {
    System.err.println("incing " + t + " to " + (t + 1));
    t = t + 1
  }

  def blockControl(todo: => Int): Int = {
    if (hasStarted && canReplay) {
      popRedo()
    } else if (!hasStarted) {
      inc()
      4
    } else {
      val r = todo
      stack(r)
      r
    }

  }

  def discard() {
    undol = Nil
  }

}

class botVocTest extends agentAbstract {

  def genMove(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)

    val currz = bv.currZones
    if (currz.size == 1) {
      System.err.println("currz\n" + currz);

      System.err.println("me\n" + bv.me);

      val dir = bv.goTo(currz(0))
      System.err.println("dir\n" + dir);
      dir(0)

    } else {
      4
    }

  }

}

class test_bv_squareUndo(dir_min45: Int, halfRad: Int, direct: Boolean) extends agentAbstract {

  var b = new test_bv_square(dir_min45, halfRad, direct)

  var logMove: log = new log
  override def backMove() {
    System.err.println("backing " + logMove);
    logMove.undo()
  }

  def genMove(ref: GameState4P) = {
    logMove.blockControl {
      b.genMove(ref)

    }

  }

}

class test_bv_square(dir_min45: Int, halfRad: Int, direct: Boolean) extends agentAbstract {

  var b: bv_followTrail = null

  def genMove(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)

    val fundir = if ((!direct & halfRad != 0) | (direct & halfRad == 0)) {
      (x: Int) => x
    } else {
      (x: Int) => 4 - x
    }

    def op(b: BMap, nb: Int): BMap = {
      if (nb == 0) b else {

        val r = dir_min45 match {
          case 0 => b.scrUL
          case 1 => b.scrUR
          case 2 => b.scrDR
          case 3 => b.scrDL
        }
        op(r, nb - 1)
      }
    }

    if (b == null) {
      b = new bv_followTrail((op(bv.me, halfRad)).border)(
        fundir)
    }
    b.genMove(ref)

  }

}

class bv_followTrail(var dst: BMap)(choicePriority: (Int) => Int)
    extends agentAbstract {
  var countMove = 0

  def genMove(ref: GameState4P) = {

    //System.err.println(""+dst);
    dst = dst & ref.tr.void
    val bv = new BotVocabulary(ref)
    dst = dst & (~bv.me)
    val dir = bv.goTo(dst)
    val r = if (dir.size >= 1) dir.maxBy(choicePriority) else 4
    r

  }
}

class bv_trailStop(var dst: BMap)(stopr: (GameState4P, BMap) => Boolean)(choicePriority: (Int) => Int = (x => x))
    extends agentAbstract {
  var countMove = 0

  def genMove(ref: GameState4P) = {

    //System.err.println(""+dst);

    if (stopr(ref, dst)) {
      4
    } else {

      dst = dst & ref.tr.void
      val bv = new BotVocabulary(ref)
      dst = dst & (~bv.me)
      val dir = bv.goTo(dst)
      val r = if (dir.size >= 1 && !stopr(ref, dst)) dir.maxBy(choicePriority) else 4
      r
    }
  }
}

class bv_zerg(dest: BMap) extends agentAbstract {
  var ra = 898;

  def genMove(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)

    //Console.err.println("frontier\n"+targ)
    val resp = bv.goTo(dest & ref.tr.void)
    ra = ((ra << 3) + 13) & 0xFFFFFF;
    def rind = ((ra % resp.size) & 3)
    if (resp.size > 0) {
      resp(rind)
    } else {
      4
    }

  }
}


class bv_tronFrontierInside extends agentAbstract {
  var ra = 898^System.nanoTime().toInt;

  def genMove(ref: GameState4P) = {

    val bv = new BotVocabulary(ref)

    def gosomewhere = {

      val targ = bv.firstTronZoneHeuristic
      //Console.err.println("raw front\n"+targ)
      val targSS = bv.border(targ._1.closeDiag)
      val targf = if (targSS.isNull) targ._2.closeDiag else targSS
      //  Console.err.println("f front\n"+targf)
      // val targfsp = targf.split
      //if (targfsp.nonEmpty) {
      //val mtarg = targfsp.maxBy { x => x.countBitset }
      val mtarg = targf
      val resp = bv.goToWithVoid(mtarg)
      ra = ((ra << 3) + 13) & 0xFFFFFF;
      def rind = ((ra % resp.size) & 3)
      if (resp.size > 0 && targ._3 < 11) {
        //   Console.err.println("Tron dist "+targ._3);
        resp(rind)
      } else {
        (ra)%5
      }
      //  } else {
      // 4
      // }
    }

    if ((ref.pos.pos0 & ref.tr.pos0).isNull) {
    //  System.err.println("FUCK " +ref.myRelScore+" Scores "+ref.scores)
      if(ref.myRelScore>0) 4 else gosomewhere
    } else {
      gosomewhere
    }
  }
}

class bv_taker(objectiveParam: BMap, seed: Long) extends agentAbstract {

  var v = seed >> 32
  var u = (seed << 32) >>> 32

  def applyRand() = {
    v = 36969 * (v & 65535) + (v >> 16);
    u = 18000 * (u & 65535) + (u >> 16);
    ((v << 16) + u) & 0xFFFF;
  }

  var nbMove = 0;

  def genMove(ref: GameState4P) = {

    val bv = new BotVocabulary(ref)
    val objective = objectiveParam | ref.tr.pos0

    if (!(ref.pos.pos0 & ref.tr.void).isNull) {
      (applyRand().toInt % 4)
    } else {

      val b = objective & (ref.tr.void | ref.tr.pos0)

      val limitsToTrait = (~(~bv.void & ~ref.tr.pos0).scramble & b.scramble)
      val limits = bv.border(limitsToTrait) & (~ref.tr.pos0) & bv.void
      val res = bv.goTo(limits)
      //System.err.println(""+limits);
      val m = if (res.size > 0) res(applyRand().toInt % res.size) else {
        // System.err.println("raw "+b);
        val res = bv.goTo(objective & ref.tr.void)
        val m = if (res.size > 0) res(applyRand().toInt % res.size) else 4
        m
      }
      m
    }
  }
}

class bv_doNothing extends agentAbstract {
  def genMove(ref: GameState4P) = {
    4
  }
}

class bv_racer extends agentAbstract {
  def genMove(ref: GameState4P) = {
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
}

class bv_tronRacer extends agentAbstract {

  var currPlan: agentAbstract = null
  var ra = 0xAA88319

  val tronIn = new bv_tronFrontierInside

  def doTronFirst(ref: GameState4P) = {

    val m = tronIn.genMove(ref)

    if (m == 4 ) {
      doPlan(ref)
    } else {
      m
    }
  }

  def doPlan(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)
    val specialVoid = (ref.tr.void | ref.tr.pos0).split
/*
    def goSomewhere = {

      val nearVoidL = (ref.pos.pos0.scramble & ref.tr.void).split

      if (nearVoidL.isEmpty) {
        val targArea = bv.border(specialVoid.maxBy { x => x.countBitset })
        val resp = bv.goTo(targArea)

        if (resp.size > 0) resp(0) else {
          4
        }
      } else {
        val nearVoids = nearVoidL.map { x => BMap.followTrail(x, ref.tr.void) }

        val targArea = bv.border(nearVoids.maxBy { x => x.countBitset })
        val resp = bv.goTo(targArea)

        if (resp.nonEmpty) {
          resp(0)
        } else 4
      }

    }*/

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
