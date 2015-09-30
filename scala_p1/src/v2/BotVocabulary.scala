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

  def squareInDir(dir: Int, sz: Int) ={
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

  def forsee_withZerger(to: BMap, plan: agentAbstract) = {
    val zerg = new bv_followTrail(to)(identity)
    val sim = new SimulBot(0, st, Array(plan, zerg, zerg, zerg))
    var dir = 0

    while (GameState4P.m(dir)(0) != 4) {
      dir = sim.turn()
    }

    sim.getState
  }

  def forsee_with(w: agentAbstract, plan: agentAbstract)(success: GameState4P => Boolean)(whenSuccess: => Int)(fail: GameState4P => Boolean)(whenFail: => Int)= {
    val zerg = w
    val sim = new SimulBot(0, st, Array(plan, zerg, zerg, zerg))
    var dir = 0
    
    var retval= -1;
    var break=false;

    while (GameState4P.m(dir)(0) != 4 && !break) {
      dir = sim.turn()
      
      val state=sim.getState
      
      if(success(state)){
        retval = whenSuccess
        break=true
      }else{
        if(fail(state)){
          retval = whenFail
          break=true
        }
        
      }      
    }
    Console.err.println(""+sim.getState);
    if(break) retval else whenFail


  }

}