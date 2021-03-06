package v2

/**
 * @author Jahan
 */

object GameState4P {

  val H = 20
  val W = 35
  val WH = H * W
  val NMOVE = 5
  val startMax: Long = WH * WH * WH * WH
  val moveMax = NMOVE * NMOVE * NMOVE * NMOVE

  // long start pos
  // long transition (625)

  def x(p: Int) = {
    p % W
  }
  def y(p: Int) = {
    p / W
  }

  def p(x: Long, i: Int): Int = {
    i match {
      case 0 => (x % WH).toInt
      case 1 => (x / (WH) % WH).toInt
      case 2 => (x / (WH * WH) % WH).toInt
      case 3 => (x / (WH * WH * WH) % WH).toInt

    }

  }

  def p(x0: Int, x1: Int, x2: Int, x3: Int) = {
    var res: Long = x3
    res = res * WH + x2
    res = res * WH + x1
    res = res * WH + x0
    res
  }

  def m(x0: Int, x1: Int, x2: Int, x3: Int) = {
    var res: Int = x3
    res = res * NMOVE + x2
    res = res * NMOVE + x1
    res = res * NMOVE + x0
    res
  }
  
  def m(x: Int)( i: Int): Int = {
    i match {
      case 0 => (x % NMOVE).toInt
      case 1 => (x / (NMOVE) % NMOVE).toInt
      case 2 => (x / (NMOVE * NMOVE) % NMOVE).toInt
      case 3 => (x / (NMOVE * NMOVE * NMOVE) % NMOVE).toInt

    }  
  }

  def start(initp: Long,nbP : Int) = {
    val init=Math.abs(initp)
    val p0 = p(init, 0)
    val p1 = p(init, 1)
    val p2 = p(init, 2)
    val p3 = p(init, 3)
    
    val m= nbP match {
      case 2 => 1
      case 3 => 3
      case 4 => 7
    }

    val st = new GameVect4P(
      BMap.zero.set(x(p0))(y(p0))(m&1),
      BMap.zero.set(x(p1))(y(p1))(m&1),
      BMap.zero.set(x(p2))(y(p2))((m>>>1)&1),
      BMap.zero.set(x(p3))(y(p3))((m>>>2)&1))

    new GameState4P(
      st, st.trace)
  }
  
  
  def convertDirToOfficialString(dir : Int, sc : Seq[Tuple3[Int,Int,Int]])={
    
    val x=sc(0)._1
    val y=sc(0)._2
    
       val out=dir match {
        case 0 => new Tuple3(x, y - 1, 0);
        case 1 => new Tuple3(x + 1, y, 0);
        case 2 => new Tuple3(x, y + 1, 0);
        case 3 => new Tuple3(x - 1, y, 0);
        case 4 => new Tuple3(x , y, 0);
        case _ => new Tuple3(0 , 0, -dir);
      }    
    
    if(out._3==0) (""+out._1+" "+out._2) else ("BACK "+(-out._3))
  }
  
  def convertDireToOfficialCoord(dir : Int)(x : Int)(y : Int)={
       val out=dir match {
        case 0 => new Tuple2(x, y - 1);
        case 1 => new Tuple2(x + 1, y);
        case 2 => new Tuple2(x, y + 1);
        case 3 => new Tuple2(x - 1, y);
        case 4 => new Tuple2(x , y);
        case _ => new Tuple2(0 , 0);
      }        
       out
  }
  
  def readOfficialGameState(sc : Seq[Tuple3[Int,Int,Int]])(sm : Seq[String]) = {
    new GameState4P(readOfficialCoords(sc),readOfficialMapTrace(sm))
  }
  
  private def readOfficialCoords(sl : Seq[Tuple3[Int,Int,Int]]) ={
    val them=Array(BMap.zero,BMap.zero,BMap.zero,BMap.zero);
    var ind=0
    
    for(t <- sl){
      if(t._1!= -1 && t._2 != -1)them(ind)=them(ind).set(t._1)(t._2)(1)
      
      ind=ind+1
    }
    
    new GameVect4P(them(0),them(1),them(2),them(3)
        )    
  }
  
  private def readOfficialMapTrace(sl : Seq[String]) = {
    val them=Array(BMap.zero,BMap.zero,BMap.zero,BMap.zero);
    

    var j=0;
    for(s <- sl){
      var i=0;
      for(c <- s){
        c match{
          case '0' => them(0)=them(0).set(i)(j)(1);
          case '1' => them(1)=them(1).set(i)(j)(1);
          case '2' => them(2)=them(2).set(i)(j)(1);
          case '3' => them(3)=them(3).set(i)(j)(1);
          case _ =>
        }
        
        i=i+1
      }
      
      j=j+1;
    }
    new GameVect4P(them(0),them(1),them(2),them(3)
        )
  }
}

class GameVect4P(
    val pos0: BMap,
    val pos1: BMap,
    val pos2: BMap,
    val pos3: BMap) {
  

  def swap(at: Int) : GameVect4P = {

    val r = at match {
      case 1 =>
        new GameVect4P(
          pos1,
          pos0,
          pos2,
          pos3)
      case 2 =>
        new GameVect4P(
          pos2,
          pos1,
          pos0,
          pos3)
      case 3 =>
        new GameVect4P(
          pos3,
          pos1,
          pos2,
          pos0)
      case _ => this
    }
    r
  }

  def applyCapture = {

    val v = void
    val nPos = new GameVect4P(
      BMap.enclosed(pos0, v),
      BMap.enclosed(pos1, v),
      BMap.enclosed(pos2, v),
      BMap.enclosed(pos3, v))

    (nPos | this)
  }

  def void = {
    var res = ~(pos0 | pos1 | pos2 | pos3)
    res
  }

  def trace = {
    var s = pos0 ^ pos1
    var c = pos0 & pos1

    c = c | (s & pos2)
    s = s ^ pos2

    c = c | (s & pos3)
    //s = s ^ pos3    

    val filt = ~c

    val res = new GameVect4P(
      pos0 & filt,
      pos1 & filt,
      pos2 & filt,
      pos3 & filt)
    res

  }

  def get(i: Int)(j: Int) = {
    val code = (pos0(i)(j)) + (pos1(i)(j) << 1) + (pos2(i)(j) << 2) + (pos3(i)(j) << 3)
    code
  }
  
  def asList={
    List(pos0,pos1,pos2,pos3)
    
  }

  def apply(i: Int) = {
    i match {
      case 0 => pos0
      case 1 => pos1
      case 2 => pos2
      case 3 => pos3
    }
  }

  override def toString = {
    var res = "";

    for (j <- 0 until GameState4P.H) {
      for (i <- 0 until GameState4P.W) {
        val v = get(i)(j).toHexString;
        val c = v match {
          case "0" => '-'
          case _   => v
        }

        res = res + c
      }
      res = res + "\n"
    }

    res
  }

  def |(t: GameVect4P) = {
    new GameVect4P(
      pos0 | t(0),
      pos1 | t(1),
      pos2 | t(2),
      pos3 | t(3))
  }

  def &(t: BMap) = {
    new GameVect4P(
      pos0 & t,
      pos1 & t,
      pos2 & t,
      pos3 & t)
  }

}

class GameState4P(
    val pos: GameVect4P,
    val tr: GameVect4P) {

  def swap(at: Int) = {
    new GameState4P(pos.swap(at), tr.swap(at))
  }

  override def toString = {
    var res = ""

    //res = res + pos.toString()
    //res = res + "--- \n"
    res = res + tr.toString()
    res = res + "=== \n"

    res

  }

  def toPosString = {
    var res = ""

    res = res + pos.toString()
    res = res + "--- \n"
    //res = res + tr.toString()
    //res = res + "=== \n"

    res

  }
  
  def scores ={
    List(tr.pos0.countBitset,tr.pos1.countBitset,tr.pos2.countBitset,tr.pos3.countBitset)
    
  }
  
  def sortedResults={
    val r= List((tr.pos0.countBitset,0),(tr.pos1.countBitset,1),(tr.pos2.countBitset,2),(tr.pos3.countBitset,3))
    r.sortBy(x=> -x._1)
  }
  
  def myRelScore={
    val h=scores.head
    h-scores.tail.minBy { x =>  h-x }
  }

  private def transitionApplyMap(m: BMap, vect: Int) = {
    val r = vect match {
      case 0 => m--
      case 1 => m>>
      case 2 => m++
      case 3 => m<<
      case 4 => m
    }

    if (r.isNull) m else r
  }

  def transition(vect: Int) = {
    val t0 = vect % 5;
    val t1 = (vect / 5) % 5;
    val t2 = (vect / (5 * 5)) % 5;
    val t3 = (vect / (5 * 5 * 5)) % 5;

    val nPos = new GameVect4P(
      transitionApplyMap(pos(0), t0),
      transitionApplyMap(pos(1), t1),
      transitionApplyMap(pos(2), t2),
      transitionApplyMap(pos(3), t3))
    val t = nPos.trace

    val ntr = (tr | (t & tr.void))

    //System.err.println(""+ntr);
    //System.err.println("capture\n"+ntr.applyCapture);

    new GameState4P(
      nPos,
      ntr.applyCapture)
  }

}