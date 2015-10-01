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

class bv_zerg(dest : BMap)  extends agentAbstract {
  var ra=898;
  
  def genMove(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)
    

    //Console.err.println("frontier\n"+targ)
    val resp = bv.goTo(dest&ref.tr.void)
    ra=((ra<<3)+13)&0xFFFFFF;
    def rind= ((ra % resp.size) & 3)
    if (resp.size > 0){
      resp(rind)
    }  else {
        4
    }

  }
}

class bv_tronFrontier extends agentAbstract {
  var ra=898;
  
  def genMove(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)
    
    val targ= bv.firstTronZoneHeuristic
   // Console.err.println("raw front\n"+targ)
    val targf= bv.border(targ)
  //  Console.err.println("f front\n"+targf)
    
    
    
    
    val resp = bv.goToWithVoid(targf)
    ra=((ra<<3)+13)&0xFFFFFF;
    def rind= ((ra % resp.size) & 3)
    if (resp.size > 0){
      resp(rind)
    }  else {
        4
    }

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
    Console.err.println("raw front\n"+targraw)
    
    if(targSplit.size>0){
      
      val mtarg=targSplit.maxBy { x => x.countBitset }
          
        val targf= bv.border(mtarg)
        Console.err.println("f front\n"+targf)
        
    
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
