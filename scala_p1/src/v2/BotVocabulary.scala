package v2


/**
 * @author Jahan
 */

class BotVocabulary(val st: GameState4P) {
  val void = st.tr.void
  val me = st.pos.pos0

  def currZones = {
    val near = me.scramble & void
    val trail = BMap.followTrail(near, void)
    trail.split

  }
  
  
  def basicCapturePathTry={
    val traits=List(0,1,2,3).map { x => (x,st.pos.pos0.shadow(st.tr.void, x)) }
    
    def dirTrail (x : Tuple2[Int,BMap])  = {
      var rotd=(x._1+1)%4
      var opd=(rotd+2)%4
      
      List( x._2, x._2.shiftIn(rotd)|st.pos.pos0.shiftIn(rotd), x._2.shiftIn(opd)|st.pos.pos0.shiftIn(opd) )
    }      
          
    traits.map(dirTrail).flatten
  }
  
  def dead_code_dirCaptureStraight ={
    val traits=List(0,1,2,3).map { x => (x,st.pos.pos0.shadow(st.tr.void, x)) }
    val capt=traits.map{ x => (x._1,
      BMap.enclosed((x._2 )  | st.tr.pos0, void & (~x._2) )
      ,x._2 ) 
    }.filter(x => x._2.notNull)
    
    capt
  }
  
  def enemies = {
    void ^ st.pos.pos0    
  }
  
  def dead_code_someCapturePlans ={
    val capt=dead_code_dirCaptureStraight
    
    def forsee(to: BMap) = {
      val zerg = new bv_followTrail(to)(identity)
      val sim = new SimulBot(0, st, Array(zerg, zerg, zerg, zerg))
      var dir = 0
  
      while (GameState4P.m(dir)(0) != 4) {
        dir = sim.turn()
      }
  
      sim.getState.myRelScore
    }
    

    
  }
  

  def firstZoneHeuristic = {
    var e = st.pos.pos1 | st.pos.pos2 | st.pos.pos3;
    var f = st.pos.pos0;

    var firste = BMap.zero;

    if (!e.isNull && !f.isNull) {
      while (!(~(f | e)).isNull) {
        e = e.scramble
        f = f.scramble
        firste = firste | (~e & f)
      }

      (firste)
    } else {
      BMap.full
    }

  }

  def extractTronZone = {
    val front = firstTronZoneHeuristic._1 | firstTronZoneHeuristic._2
    val res = BMap.followTrail(st.pos.pos0, ~front)
    res
  }

  def shadows = {
    def exts(m : BMap) = {
     val s= for(b <- List(0,1,2,3)) yield{
        val shad=m.shadow(void, b)
        shad.split
      }          
      s.flatten
    }
    
    st.tr.asList.map { x => exts(x) }
    
  }

  def extractAllZone = {
    List(extractTronZone,
        (new BotVocabulary(st.swap(1))).extractTronZone,
        (new BotVocabulary(st.swap(2))).extractTronZone,
        (new BotVocabulary(st.swap(3))).extractTronZone).filter { x => !x.isNull }

  }

  def firstTronZoneHeuristic: Tuple3[BMap, BMap, Int] = {
    var e = st.pos.pos1 | st.pos.pos2 | st.pos.pos3;
    var f = st.pos.pos0;
    var v = st.tr.void

    var dist = 0
    var countIt = 0

    def collide = {
      ((!(e & f).isNull)) || (!(e.scramble & f).isNull)
    }

    //  Console.err.println("start v\n"+v);
    //   Console.err.println("start e\n"+e);
    //  Console.err.println("start f\n"+f);

    var firste = BMap.zero;
    var last = BMap.full

    if (!e.isNull && !f.isNull) {
      while (!((v ^ last).isNull)) {
        e = (e.scramble & v) | e
        f = (f.scramble & v) | f

        //Console.err.println(" e\n"+e);
        //Console.err.println(" f\n"+f);        
        //Console.err.println(" v & ((~e) | (~f))\n"+( v &  ((~e) & (~f))));
        last = v
        v = (v & ((~e) & (~f)))

        // Console.err.println(" v\n"+v);   
        firste = firste | (e & f)
        // Console.err.println("firste\n"+firste);
        // Console.err.println("vide\n"+v);

        if (dist == 0 && collide) {
          dist = countIt
        }

        countIt = countIt + 1
      }
      {
        (firste, (e.scramble & f) | (f.scramble & e) & st.tr.void, dist)
      }

    } else {
      (BMap.full, BMap.full, 0)
    }

  }

  def border(area: BMap) = {
    ((area | st.tr.pos0).border) & (~st.tr.pos0)
  }

  def simpleSquareRuleZone = {
    val ref = st
    val bv = new BotVocabulary(ref)
    val first = bv.firstZoneHeuristic
    val void = ref.tr.void

    val allFirstEmpty = (first & void).split
    val maxfirst = if (allFirstEmpty.size > 0) { allFirstEmpty.maxBy { x => x.countBitset } }
    else {
      if (void.isNull) {
        void
      } else {
        void.split.maxBy { x => x.countBitset }
      }
    }

    val area = (bv.nthBm(maxfirst.noyau, 3) { x => x.angularScramble }) & (void | ref.tr.pos0)
    (area & (first & void))
  }

  def goTo(to: BMap) = {
    BMap.firstDirTo(me, to)
  }

  def goToElseGo(to: BMap)(elsego: => Int): Int = {

    if (to.isNull) {
      elsego
    } else {
      val resp = goTo(to)

      if (resp.size > 0) resp(0) else {
        elsego
      }
    }
  }

  def goToWithVoid(to: BMap) = {
    BMap.firstDirToThrough(me, to, st.tr.void)
  }

  def direction(dir: Int) = {
    val r = dir match {
      case 0 => st.pos.pos0--
      case 1 => st.pos.pos0>>
      case 2 => st.pos.pos0++
      case 3 => st.pos.pos0<<
      case 4 => st.pos.pos0
    }
    r
  }

  def squareInDir(dir: Int, sz: Int) = {
    def squareInDirRec(start: BMap, dir: Int, sz: Int): BMap = {

      if (sz == 0) start else {

        val r = dir match {
          case 0 => start.scrUL
          case 1 => start.scrUR
          case 2 => start.scrDR
          case 3 => start.scrDL
        }
        squareInDirRec(r, dir, sz - 1)
      }
    }
    squareInDirRec(me, dir, sz)
  }

  def testDirVoid(dir: Int) = {
    !(direction(dir) & st.tr.void).isNull
  }

  def nthBm(to: BMap, nb: Int)(code: BMap => BMap): BMap = {
    if (nb == 0) to else {
      nthBm(code(to), nb - 1)(code)
    }
  }
  
  
  def eval_withTaker(to : BMap){
    def zerg ={
      new bv_taker(to,0xFF93887492L)
    }
    
    val sim = new SimulBot(0, st, Array(zerg, zerg, zerg, zerg))
    var dir = 0
    var nbMove=0
    while (((sim.getState.tr.void&to ).notNull) && nbMove < 200) {
       nbMove=nbMove+1
      
      dir = sim.turn()
      //System.err.println("Evaluating \n"+sim.getState);
    }

    val res=sim.getState.myRelScore
    
  //  System.err.println("Evaled with takers "+res);
    
  }

  def forsee_withZerger(to: BMap, plan: agentAbstract) = {
    val zerg = new bv_followTrail(to)(identity)
    val sim = new SimulBot(0, st, Array(plan, zerg, zerg, zerg))
    var dir = 0

    while (GameState4P.m(dir)(0) != 4) {
      dir = sim.turn()
    }

    sim.getState
  }

  def forsee_with(w: agentAbstract, plan: agentAbstract)(success: GameState4P => Boolean)(whenSuccess: => Int)(fail: GameState4P => Boolean)(whenFail: => Int) = {
    val zerg = w
    val sim = new SimulBot(0, st, Array(plan, zerg, zerg, zerg))
    var dir = 0

    var retval = -1;
    var break = false;

    while (GameState4P.m(dir)(0) != 4 && !break) {
      dir = sim.turn()

      val state = sim.getState

      if (success(state)) {
        retval = whenSuccess
        break = true
      } else {
        if (fail(state)) {
          retval = whenFail
          break = true
        }

      }
    }
    //Console.err.println(""+sim.getState);
    if (break) retval else whenFail

  }

}