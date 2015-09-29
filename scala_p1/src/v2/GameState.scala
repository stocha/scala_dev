package v2

/**
 * @author Jahan
 */

object GameState4P {

  val H = 20
  val W = 35
  val WH = H * W
  val NMOVE=5  
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

  def start(init: Long) = {
    val p0 = p(init, 0)
    val p1 = p(init, 1)
    val p2 = p(init, 2)
    val p3 = p(init, 3)

    val st = new GameVect4P(
      BitMap.zero.set(x(p0))(y(p0))(1),
      BitMap.zero.set(x(p1))(y(p1))(1),
      BitMap.zero.set(x(p2))(y(p2))(1),
      BitMap.zero.set(x(p3))(y(p3))(1))

    new GameState4P(
      st, st.trace)
  }
}

class GameVect4P(
    val pos0: BitMap,
    val pos1: BitMap,
    val pos2: BitMap,
    val pos3: BitMap) {
  
  
  def applyCapture={
    
    val v=void
    val nPos= new GameVect4P(
          BitMap.enclosed(pos0,v ),
          BitMap.enclosed(pos1,v ),
          BitMap.enclosed(pos2,v ),
          BitMap.enclosed(pos3,v )
        )    
    
    ( nPos | this)
  }
  
  
  def void ={
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
        val c = v match{
          case "0" => '-'
          case _ => v
        }
        
        res = res + c
      }
      res = res + "\n"
    }

    res
  }
  
  def | (t : GameVect4P)={
    new GameVect4P(
      pos0|t(0),
      pos1|t(1),
      pos2|t(2),
      pos3|t(3)
    )
  }
  
  def & (t : BitMap) ={
    new GameVect4P(
      pos0 & t,
      pos1 & t,
      pos2 & t,
      pos3 & t
    )    
  }

}

class GameState4P(
    val pos: GameVect4P,
    val tr: GameVect4P) {

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
 

  private def transitionApplyMap(m: BitMap, vect: Int) = {
    val r=vect match {
      case 0 => m--
      case 1 => m>>
      case 2 => m++
      case 3 => m<<
      case 4 => m
    }
    
    if(r.isNull) m else r
  }

  def transition(vect: Int) ={
    val t0 = vect % 5;
    val t1 = (vect / 5) % 5;
    val t2 = (vect / (5*5)) % 5;
    val t3 = (vect / (5*5*5)) % 5;
    
    val nPos= new GameVect4P(
          transitionApplyMap(pos(0),t0 ),
          transitionApplyMap(pos(1),t1 ),
          transitionApplyMap(pos(2),t2 ),
          transitionApplyMap(pos(3),t3 )
        )
    val t=nPos.trace
    
    val ntr=(tr | (t & tr.void))
    
    //System.err.println(""+ntr);
    //System.err.println("capture\n"+ntr.applyCapture);
    
    new GameState4P(
          nPos,
          ntr.applyCapture
        )
  }
  
}