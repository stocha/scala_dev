package other.v2



object BMap {

  val zero = {
    new BMap(
      0,
      0,
      0,
      0,
      0,

      0,
      0,
      0,
      0,
      0,

      0,
      0,
      0,
      0,
      0,

      0,
      0,
      0,
      0,
      0)

  }

  val umask = {
    (-1L) >>> (64 - 35);
  }

  val full = {
    new BMap(
      umask,
      umask,
      umask,
      umask,
      umask,

      umask,
      umask,
      umask,
      umask,
      umask,

      umask,
      umask,
      umask,
      umask,
      umask,

      umask,
      umask,
      umask,
      umask,
      umask)
  }

  val border = {
    var f = full;
    f = f<<-;
    f = f>>+;
    f = f>>+;

    f = f<<-;

    ~f

  }

  def enclosed(friend: BMap, void: BMap) = {
    var e = border | ~void;
    var f = friend;

    // println("f \n" + f);
    // println("v \n" + void);

    e = e & ~f

    var check = e;
    var oldcheck = zero;

    while (!(check ^ oldcheck).isNull) {
      oldcheck = check

      check = (check | (oldcheck>>-))
      check = (check | (oldcheck>>+))
      check = (check | (oldcheck<<-))
      check = (check | (oldcheck<<+))
      check = (check | (oldcheck>>))
      check = (check | (oldcheck<<))
      check = (check | (oldcheck--))
      check = (check | (oldcheck++))

      check = (check & void)

      //println("check boucle  \n" + check);
    }
    //println("not f  \n" + void);
    //println("check  \n" + check);
    val res = (~check ^ f)
    //println("check result \n" + res);
    res & void

  }

  def voidArea(all: Array[BMap]) = {
    var void = zero;

    for (i <- 0 until all.size) {
      void = void | all(i)
    }
    ~void
  }

  def closeDiag(frontier: BMap, void: BMap) = {
    val ul = ((frontier--) & (frontier<<)) & ~(frontier<<-)
    val ur = ((frontier--) & (frontier>>)) & ~(frontier>>-)
    val dl = ((frontier++) & (frontier<<)) & ~(frontier<<+)
    val dr = ((frontier++) & (frontier>>)) & ~(frontier>>+)

    ((ul | ur | dl | dr) & void)
  }

  def followTrail(pos: BMap, trail: BMap) = {
    var curr = pos.scramble & trail;
    var last = BMap.zero

    while (!(curr ^ last).isNull) {
      last = curr
      curr = curr.scramble & trail
    }

    curr
  }

  def firstDirToThrough(pos: BMap, goal: BMap, conduction: BMap) = {

    var last = zero
    var curr = goal

    var res = List[Int]();

    val conduct = conduction | pos;

    // Console.err.println("begin search to\n"+goal );
    while (!(last ^ curr).isNull && res.size == 0) {
      last = curr
      val up = (curr++) & conduct
      val down = (curr--) & conduct
      val left = (curr>>) & conduct
      val right = (curr<<) & conduct

      //Console.err.println("(up & pos)\n"+(up & pos) );
      //Console.err.println("(right & pos)\n"+(right & pos) );
      //Console.err.println("(down & pos)\n"+(down & pos) );
      //Console.err.println("(left & pos)\n"+(left & pos) );

      if (!(up & pos).isNull) res = 0 :: res;
      if (!(right & pos).isNull) res = 1 :: res;
      if (!(down & pos).isNull) res = 2 :: res;
      if (!(left & pos).isNull) res = 3 :: res;

      curr = curr | up
      curr = curr | down
      curr = curr | left
      curr = curr | right

      //Console.err.println("curr\n"+curr );
    }
    res
  }

  def firstDirTo(pos: BMap, goal: BMap) = {
    firstDirToThrough(pos, goal, full)
  }
}

class BMap(
    val u00: Long,
    val u01: Long,
    val u02: Long,
    val u03: Long,
    val u04: Long,

    val u05: Long,
    val u06: Long,
    val u07: Long,
    val u08: Long,
    val u09: Long,

    val u10: Long,
    val u11: Long,
    val u12: Long,
    val u13: Long,
    val u14: Long,

    val u15: Long,
    val u16: Long,
    val u17: Long,
    val u18: Long,
    val u19: Long //,       
    //val end : Long
    ) {

  def isNull = {
    var acc = 0L

    acc = acc | u00
    acc = acc | u01
    acc = acc | u02
    acc = acc | u03
    acc = acc | u04

    acc = acc | u05
    acc = acc | u06
    acc = acc | u07
    acc = acc | u08
    acc = acc | u09

    acc = acc | u10
    acc = acc | u11
    acc = acc | u12
    acc = acc | u13
    acc = acc | u14

    acc = acc | u15
    acc = acc | u16
    acc = acc | u17
    acc = acc | u18
    acc = acc | u19

    (acc == 0)
  }

  def split = {
    extractZones

  }

  def border = {
    val ext = this.scramble ^ this;
    (ext.angularScramble & this) | (this&BMap.border)
  }

  def l_getAt(at: Int): Long = {
    at match {
      case 0  => u00
      case 1  => u01
      case 2  => u02
      case 3  => u03
      case 4  => u04

      case 5  => u05
      case 6  => u06
      case 7  => u07
      case 8  => u08
      case 9  => u09

      case 10 => u10
      case 11 => u11
      case 12 => u12
      case 13 => u13
      case 14 => u14

      case 15 => u15
      case 16 => u16
      case 17 => u17
      case 18 => u18
      case 19 => u19
    }
  }

  def l_setAt(at: Int)(vvv: Long): BMap = {

    //new BMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
    at match {
      case 0  => new BMap(vvv, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 1  => new BMap(u00, vvv, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 2  => new BMap(u00, u01, vvv, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 3  => new BMap(u00, u01, u02, vvv, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 4  => new BMap(u00, u01, u02, u03, vvv, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)

      case 5  => new BMap(u00, u01, u02, u03, u04, vvv, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 6  => new BMap(u00, u01, u02, u03, u04, u05, vvv, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 7  => new BMap(u00, u01, u02, u03, u04, u05, u06, vvv, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 8  => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, vvv, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 9  => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, vvv, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)

      case 10 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, vvv, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 11 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, vvv, u12, u13, u14, u15, u16, u17, u18, u19)
      case 12 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, vvv, u13, u14, u15, u16, u17, u18, u19)
      case 13 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, vvv, u14, u15, u16, u17, u18, u19)
      case 14 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, vvv, u15, u16, u17, u18, u19)

      case 15 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, vvv, u16, u17, u18, u19)
      case 16 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, vvv, u17, u18, u19)
      case 17 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, vvv, u18, u19)
      case 18 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, vvv, u19)
      case 19 => new BMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, vvv)
    }
  }

  def scramble = {
    var check = this;
    val oldcheck = check;

    check = (check | (oldcheck>>))
    check = (check | (oldcheck<<))
    check = (check | (oldcheck--))
    check = (check | (oldcheck++))

    check
  }

  def angularScramble = {
    var check = this;
    val oldcheck = check;

    check = (check | (oldcheck>>-))
    check = (check | (oldcheck>>+))
    check = (check | (oldcheck<<-))
    check = (check | (oldcheck<<+))
    check = (check | (oldcheck>>))
    check = (check | (oldcheck<<))
    check = (check | (oldcheck--))
    check = (check | (oldcheck++))

    check
  }

  def scrUL = {
    var check = this;
    val oldcheck = check;

    check = (check | (oldcheck<<-))
    check = (check | (oldcheck<<))
    check = (check | (oldcheck--))

    check
  }

  def scrUR = {
    var check = this;
    val oldcheck = check;

    check = (check | (oldcheck>>-))
    check = (check | (oldcheck>>))
    check = (check | (oldcheck--))

    check
  }

  def scrDL = {
    var check = this;
    val oldcheck = check;

    check = (check | (oldcheck<<+))
    check = (check | (oldcheck<<))
    check = (check | (oldcheck++))

    check
  }

  def scrDR = {
    var check = this;
    val oldcheck = check;

    check = (check | (oldcheck>>+))
    check = (check | (oldcheck>>))
    check = (check | (oldcheck++))

    check
  }

  def l_toString(at: Int) = {
    val r = l_getAt(at)
    llong_toString(r)
  }

  def llong_toString(r: Long) = {
    var res = "";

    for (i <- 0 until 35) {
      val c = if (((r >>> i) & 1L) == 0) "- " else "# ";
      res += c;
    }
    res
  }

  def get(x: Int)(y: Int) = {
    val r = l_getAt(y)
    (r >>> x) & 1L
  }

  def apply(x: Int)(y: Int) = {
    get(x)(y)
  }

  def set(x: Int)(y: Int)(v: Long) = {
    val r: Long = l_getAt(y)
    val b: Long = (1L << x)
    val nb: Long = (~b & r) | ((v << x) & b)
    //println("old "+llong_toString(r));
    //println("new "+llong_toString(nb));
    val res = l_setAt(y)(nb)
    //println("res is \n"+res);
    res
  }

  override def toString() = {
    var res = "";

    for (i <- 0 until 20) {
      res += l_toString(i);
      res += "\n";
    }
    res
  }

  def unary_~ = {
    new BMap(BMap.umask & ~u00, BMap.umask & ~u01, BMap.umask & ~u02, BMap.umask & ~u03, BMap.umask & ~u04,
      BMap.umask & ~u05, BMap.umask & ~u06, BMap.umask & ~u07, BMap.umask & ~u08, BMap.umask & ~u09,
      BMap.umask & ~u10, BMap.umask & ~u11, BMap.umask & ~u12, BMap.umask & ~u13, BMap.umask & ~u14,
      BMap.umask & ~u15, BMap.umask & ~u16, BMap.umask & ~u17, BMap.umask & ~u18, BMap.umask & ~u19)

  }

  def forAllSet(blockOfCode: (Int, Int) => Unit) {
    for (i <- 0 until 35; j <- 0 until 20) {
      if (get(i)(j) != 0) {
        blockOfCode(i, j)
      }
    }
  }

  def firstSetBitBm = {

    def firstInLong(l: Long) = {
      ((l - 1) ^ l) & l
    }

    def recLook(h: Int): BMap = {
      if (h < 0) {
        BMap.zero
      } else {
        val lat = l_getAt(h)
        if (lat != 0) {
          BMap.zero.l_setAt(h)(firstInLong(lat))
        } else {
          recLook(h - 1)
        }
      }
    }

    recLook(19)
  }

  def countBitset = {
    var add = 0;
    forAllSet((_, _) => add = add + 1)
    add
  }

  def extractZones = {

    var res: List[BMap] = Nil

    var suivi = this

    while (!suivi.isNull) {
      val bit = suivi.firstSetBitBm
      val fill = BMap.followTrail(bit, suivi)

      res = fill :: res
      suivi = suivi ^ fill

    }
    res

  }

  def &(that: BMap) = {
    new BMap(
      u00 & that.u00,
      u01 & that.u01,
      u02 & that.u02,
      u03 & that.u03,
      u04 & that.u04,
      u05 & that.u05,
      u06 & that.u06,
      u07 & that.u07,
      u08 & that.u08,
      u09 & that.u09,
      u10 & that.u10,
      u11 & that.u11,
      u12 & that.u12,
      u13 & that.u13,
      u14 & that.u14,
      u15 & that.u15,
      u16 & that.u16,
      u17 & that.u17,
      u18 & that.u18,
      u19 & that.u19)
  }

  def |(that: BMap) = {
    new BMap(
      u00 | that.u00,
      u01 | that.u01,
      u02 | that.u02,
      u03 | that.u03,
      u04 | that.u04,
      u05 | that.u05,
      u06 | that.u06,
      u07 | that.u07,
      u08 | that.u08,
      u09 | that.u09,
      u10 | that.u10,
      u11 | that.u11,
      u12 | that.u12,
      u13 | that.u13,
      u14 | that.u14,
      u15 | that.u15,
      u16 | that.u16,
      u17 | that.u17,
      u18 | that.u18,
      u19 | that.u19)
  }

  def ^(that: BMap) = {
    new BMap(
      u00 ^ that.u00,
      u01 ^ that.u01,
      u02 ^ that.u02,
      u03 ^ that.u03,
      u04 ^ that.u04,
      u05 ^ that.u05,
      u06 ^ that.u06,
      u07 ^ that.u07,
      u08 ^ that.u08,
      u09 ^ that.u09,
      u10 ^ that.u10,
      u11 ^ that.u11,
      u12 ^ that.u12,
      u13 ^ that.u13,
      u14 ^ that.u14,
      u15 ^ that.u15,
      u16 ^ that.u16,
      u17 ^ that.u17,
      u18 ^ that.u18,
      u19 ^ that.u19)
  }

  def ++ = {
    new BMap(0L, u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18)

  }

  def -- = {
    new BMap(u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, 0L)
  }

  def << = {
    //System.err.println("apply << ");
    new BMap(u00 >>> 1, u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1, u19 >>> 1)
  }

  def >> = {
    new BMap(BMap.umask & u00 << 1, BMap.umask & u01 << 1, BMap.umask & u02 << 1, BMap.umask & u03 << 1, BMap.umask & u04 << 1, BMap.umask & u05 << 1, BMap.umask & u06 << 1, BMap.umask & u07 << 1, BMap.umask & u08 << 1, BMap.umask & u09 << 1, BMap.umask & u10 << 1, BMap.umask & u11 << 1, BMap.umask & u12 << 1, BMap.umask & u13 << 1, BMap.umask & u14 << 1, BMap.umask & u15 << 1, BMap.umask & u16 << 1, BMap.umask & u17 << 1, BMap.umask & u18 << 1, BMap.umask & u19 << 1)
  }

  def <<- = {
    new BMap(u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1, u19 >>> 1, 0L)

  }

  def <<+ = {
    new BMap(0L, u00 >>> 1, u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1)

  }

  def >>- = {
    new BMap(BMap.umask & u01 << 1, BMap.umask & u02 << 1, BMap.umask & u03 << 1, BMap.umask & u04 << 1, BMap.umask & u05 << 1, BMap.umask & u06 << 1, BMap.umask & u07 << 1, BMap.umask & u08 << 1, BMap.umask & u09 << 1, BMap.umask & u10 << 1, BMap.umask & u11 << 1, BMap.umask & u12 << 1, BMap.umask & u13 << 1, BMap.umask & u14 << 1, BMap.umask & u15 << 1, BMap.umask & u16 << 1, BMap.umask & u17 << 1, BMap.umask & u18 << 1, BMap.umask & u19 << 1, 0L)
  }

  def >>+ = {
    new BMap(0L, BMap.umask & u00 << 1, BMap.umask & u01 << 1, BMap.umask & u02 << 1, BMap.umask & u03 << 1, BMap.umask & u04 << 1, BMap.umask & u05 << 1, BMap.umask & u06 << 1, BMap.umask & u07 << 1, BMap.umask & u08 << 1, BMap.umask & u09 << 1, BMap.umask & u10 << 1, BMap.umask & u11 << 1, BMap.umask & u12 << 1, BMap.umask & u13 << 1, BMap.umask & u14 << 1, BMap.umask & u15 << 1, BMap.umask & u16 << 1, BMap.umask & u17 << 1, BMap.umask & u18 << 1)
  }

  def paintCrossAt(x: Int, y: Int) = {
    var res = l_setAt(y)(-1L)
    for (i <- 0 until 20) {
      res = res.set(x)(i)(1)
    }
    res
  }

  def noyau = {
    var last = ~this
    var curr = (~this).scramble | BMap.border;

    while (!(~curr).isNull) {
      last = curr
      curr = curr.scramble
    }

    ~last
  }

  def closestPointHere(from: BMap) = {
    var dist = 0;
    var curr = from;

    val thisNotNull = !this.isNull && !from.isNull
    while (((curr & this).isNull) & (thisNotNull)) {
      curr = curr | curr.scramble
      dist = dist + 1
      // Console.err.println("curr\n"+curr);
    }

    new Tuple2(dist, curr & this)
  }

}


// @author Jahan

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

  def start(init: Long) = {
    val p0 = p(init, 0)
    val p1 = p(init, 1)
    val p2 = p(init, 2)
    val p3 = p(init, 3)

    val st = new GameVect4P(
      BMap.zero.set(x(p0))(y(p0))(1),
      BMap.zero.set(x(p1))(y(p1))(1),
      BMap.zero.set(x(p2))(y(p2))(1),
      BMap.zero.set(x(p3))(y(p3))(1))

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
 
class BotVocabulary(val  st : GameState4P) {
  val void = st.tr.void
  val me = st.pos.pos0
  
  
  def currZones = {
    val near= me.scramble & void
    val trail=BMap.followTrail(near, void)
    trail.split
    
  }
  
  
  def firstZoneHeuristic = {
    var e = st.pos.pos1 | st.pos.pos2 | st.pos.pos3;
    var f = st.pos.pos0;

    var firste = BMap.zero;
    
    if(!e.isNull && !f.isNull){
      while (!(~(f | e)).isNull) {
        e = e.scramble
        f = f.scramble
        firste = firste | (~e & f)
      }
  
      (firste )      
    }else{
      BMap.full
    }
        

  }    
  
  def goTo(to : BMap) = {
    BMap.firstDirTo(me, to)
  }
  
  
  def nthBm(to : BMap, nb : Int)(code : BMap => BMap) : BMap={
    if(nb ==0) to else{
      nthBm( code(to) , nb-1)(code)
    }
  }
  
  
}


class log{
  private var donel : List[Int] = List()
  private var undol : List[Int] = List()
  private var t=0
  
  override def toString={
    var res =""
    res =  res + t+"  "+donel+"   /    "+undol
    
    res
  }
  
  private def stack(v : Int){
    donel = v :: donel
    t=  t+1
  }
  
  private def canReplay = {
    !undol.isEmpty
  }
  
  def hasStarted ={
    t>=0
  }
  
  def undo(){
    if(t>0){
    
      undol= donel.head::undol
      donel = donel.tail
    }
    t=t-1
  }
  
  private def popRedo()={
    t=t+1
    val r = undol.head;
    donel = undol.head::donel
    undol = undol.tail
    r
  }
  
  private def inc(){
    System.err.println("incing "+t+" to "+(t+1));
    t=t+1
  }
  
  def blockControl(todo : =>Int ) : Int ={
      if(hasStarted && canReplay){
        popRedo()
      }else if(!hasStarted){
        inc()
        4
      }else
      {
       val r= todo
       stack(r)
       r
      }    
    

  }
  
  def discard(){
    undol=Nil
  }
  
  
  
}

class score(val x0 : Int,val x1 : Int,val x2 : Int,val x3 : Int){
  override def toString={
    val res="("+x0+" / "+x1+" / "+x2+" / "+x3+")"
    
    res
  }
  
}


  abstract class agentAbstract{
    def genMove (ref : GameState4P) : Int    
    def backMove(){}
  }  


class SimulBot (seed : Long , val  ref : GameState4P,val agents : Array[agentAbstract]) {
  private var sim=ref
 // private val agents = Array(new stupidAgent,new stupidAgent,new stupidAgent,new stupidAgent)
  
  var log=List[Int]()
  
  def apply(i : Int )(j : Int) ={
    sim.tr.get(i)(j)
  }  
  
  def reset(){
    sim=ref
  }
  
  def turn()={
      val d0=agents(0).genMove(sim.swap(0))
      val d1=      if(agents.size>1) agents(1).genMove(sim.swap(1)) else 4
      val d2=      if(agents.size>2) agents(2).genMove(sim.swap(2)) else 4
      val d3=       if(agents.size>3) agents(3).genMove(sim.swap(3)) else 4
      val move =GameState4P.m(d0, d1, d2, d3)
      sim= sim.transition(move)
      //System.err.println("Turn "+i);
      //System.err.println(""+game);    
      log = move :: log
      move
  }
  
  def doBackward(){
    
    if(!log.isEmpty){
      log= log.tail
      sim=ref
      
      for(i <- log.reverse){
        //System.err.println(""+i);
        sim=sim.transition(i)
      }
    }
    
    for(b <- agents){
      //System.err.println("backing up");
      b.backMove()
    }
  }
  
  def eval()={
    for(i<-0 until 350){
      turn()
    }
    
    new score(sim.tr(0).countBitset,sim.tr(1).countBitset,sim.tr(2).countBitset,sim.tr(3).countBitset)
  }
    
  
  def getState={
    
    sim
  }
    
}


class botVocTest extends agentAbstract{
  
    def genMove (ref : GameState4P) ={
      val bv = new BotVocabulary(ref)
      
      val currz=bv.currZones
      if(currz.size==1){
        System.err.println("currz\n"+currz);
        
        System.err.println("me\n"+bv.me);
        
        val dir =bv.goTo(currz(0))
        System.err.println("dir\n"+dir);
        dir(0)
        
        
      }else{
        4
      }

    }
  
}

class test_bv_squareUndo(dir_min45 : Int, halfRad : Int , direct : Boolean) extends agentAbstract{
  
    var b =new test_bv_square(dir_min45,halfRad,direct)
  
    var logMove : log = new log
    override def backMove(){
      System.err.println("backing "+logMove);
      logMove.undo()
    }
    
      def genMove (ref : GameState4P) ={
        logMove.blockControl{
          b.genMove(ref)
          
        }
      
      }
  
}

class test_bv_square(dir_min45 : Int, halfRad : Int , direct : Boolean) extends agentAbstract{
  
  var b : bv_followTrail = null
  
  def genMove (ref : GameState4P) ={
      val bv = new BotVocabulary(ref)
      
      val fundir = if((!direct & halfRad!=0) | (direct & halfRad==0)){
         (x : Int) =>  x 
      }else
      {
        (x : Int) => 4- x
      }
      
      def op(b: BMap, nb : Int) : BMap ={
        if(nb==0) b else
        {
        
          val r=dir_min45 match{
            case 0 => b.scrUL
            case 1 => b.scrUR
            case 2 => b.scrDR
            case 3 => b.scrDL
          }
          op(r,nb-1)
        }
        
      }
      
      if(b==null){
        b = new bv_followTrail((op ( bv.me,halfRad)   ).border)(
          fundir
        )
      }
      b.genMove(ref)

    }    
  
}


class bv_followTrail(var  dst :BMap ) ( choicePriority : (Int)=>Int)
extends agentAbstract{
    var countMove=0

  
    def genMove (ref : GameState4P) ={
      
      //System.err.println(""+dst);
        val bv = new BotVocabulary(ref)      
        dst= dst & (~bv.me)      
        val dir =bv.goTo(dst)
        val r=if(dir.size>=1) dir.maxBy(choicePriority) else 4        
        r      
      
    }
    
    

  
}


object Player extends App {

  
  val opponentcount = readInt // Opponent count
  
  val bot=new tb001
  
  var date=0;

  // game loop
  while (true) {
    val gameround = readInt
    // x: Your x position
    // y: Your y position
    // backintimeleft: Remaining back in time
    
    if(gameround<=date){
      while(date>=gameround){
          Console.err.println("back to "+gameround+" / "+date);
        date=date-1
        bot.backMove()
      }
    }
    date=date+1
    
    
    val someCoord = for (i <- 0 until (opponentcount + 1)) yield {
      // opponentx: X position of the opponent
      // opponenty: Y position of the opponent
      // opponentbackintimeleft: Remaining back in time of the opponent
      val Array(opponentxb, opponentyb, opponentbackintimeleft) = for (i <- readLine split " ") yield i.toInt
      val opponentx = if(opponentxb>0) opponentxb else 0;
      val opponenty = if(opponentyb>0) opponentyb else 0;
      
      new Tuple3(opponentx, opponenty, opponentbackintimeleft)

    }

    val ls = for (i <- 0 until 20) yield {
      val line = readLine // One line of the map ('.' = free, '0' = you, otherwise the id of the opponent)
      line
    }
    
    val t0 = System.nanoTime()

    
    val map=GameState4P.readOfficialGameState(someCoord)(ls)
    
          
    
    
        
    
    val order=bot.genMove(map)
    
      val t1 = System.nanoTime()
      
      var t : Double =( t1 - t0) / 1000
      Console.err.println(" "+t+" nanoseconds "+(t/1000)+" millisecondes");    

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

        println( GameState4P.convertDirToOfficialString(order,someCoord))
  }
}


class tb001() extends agentAbstract{
  
  
   var currPlan : agentAbstract=null
  
  def doPlan(ref : GameState4P) ={
  
    val bv = new BotVocabulary(ref)
    val first=bv.firstZoneHeuristic
    val void =ref.tr.void
    
    val allFirstEmpty=(first&void).split
    val maxfirst= if(allFirstEmpty.size>0) {allFirstEmpty.maxBy { x => x.countBitset } }
     else{
       if(void.isNull){
         void
       }else{
         void.split.maxBy { x => x.countBitset }
       }
     }
    
    val area=(bv.nthBm(maxfirst.noyau, 3){x => x.angularScramble })&(void|bv.me)
    //Console.err.println("target area\n"+area);
    //Console.err.println("target border\n"+area.border);
    
    currPlan=new bv_followTrail(area.border) (x => x)
    
    currPlan.genMove(ref)
  }
  
  
  
    var logMove : log = new log
    override def backMove(){
     // System.err.println("backing "+logMove);
      logMove.undo()
    }
    
      def genMove (ref : GameState4P) ={
        logMove.blockControl{
          
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