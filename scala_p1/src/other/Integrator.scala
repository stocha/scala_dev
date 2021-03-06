package other
import scala.util.Random


object ServWorld {
  val W = 35
  val H = 20
  val DUR2 = 350
  val DUR3 = 300
  val DUR4 = 250
  val MAXBACK = 25

}



object BitMap {

  val zero = {
    new BitMap(
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
    new BitMap(
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

  def enclosed(all: Array[BitMap], id: Int) = {
    var e = border;
    var f = zero;

    for (i <- 0 until all.size) {
      if (i == id) {
        f = f | all(i)
      } else {
        e = e | all(i)
      }
    }
    val void = ~f;
    e = e & void

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
    res

  }

  def enclosed(friend: BitMap, void: BitMap) = {
    var check = (~void | border) & ~friend;
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
    val res = (~check ^ friend)
    //println("check result \n" + res);
    res & void

  }

  def voidArea(all: Array[BitMap]) = {
    var void = zero;

    for (i <- 0 until all.size) {
      void = void | all(i)
    }
    ~void
  }

  def closeDiag(frontier: BitMap, void: BitMap) = {
    val ul = ((frontier--) & (frontier<<)) & ~(frontier<<-)
    val ur = ((frontier--) & (frontier>>)) & ~(frontier>>-)
    val dl = ((frontier++) & (frontier<<)) & ~(frontier<<+)
    val dr = ((frontier++) & (frontier>>)) & ~(frontier>>+)

    ((ul | ur | dl | dr) & void)
  }

  def followTrail(pos: BitMap, trail: BitMap) = {
    var curr = pos.scramble & trail;
    var last = BitMap.zero

    while (!(curr ^ last).isNull) {
      last = curr
      curr = curr.scramble & trail
    }

    curr
  }

  def firstArea(all: Array[BitMap], pos: Array[servCoord], id: Int) = {
    var e = zero;
    var f = zero;
    var void = zero;

    var firste = zero;

    for (i <- 0 until all.size) {
      void = void | all(i)
      if (i == id) {
        val c = pos(i)
        f = f.set(c.x)(c.y)(1)
      } else {
        val c = pos(i)
        if (c.x != -1 && c.y != -1)
          e = e.set(c.x)(c.y)(1)
      }
    }
    void = ~void

    while (!(~(f | e)).isNull) {
      e = e.scramble
      f = f.scramble

      //println("e\n"+e);

      //println("\nf\n"+f);

      firste = firste | (~e & f)
    }

    (firste & void)
  }
  

  def rawFirstMap(pos: Array[servCoord], id: Int) = {
    var e = zero;
    var f = zero;

    var firste = zero;

    for (i <- 0 until pos.size) {
      if (i == id) {
        val c = pos(i)
        f = f.set(c.x)(c.y)(1)
      } else {
        val c = pos(i)
        if (c.x != -1 && c.y != -1)
          e = e.set(c.x)(c.y)(1)
      }
    }
    while (!(~(f | e)).isNull) {
      e = e.scramble
      f = f.scramble
      firste = firste | (~e & f)
    }

    (firste )
  }  

  def firstDirToThrough(pos: BitMap, goal: BitMap, conduction: BitMap) = {

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

  def firstDirTo(pos: BitMap, goal: BitMap) = {
    firstDirToThrough(pos, goal, full)
  }
}

class BitMap(
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

  def frontierMap = {
    val ext = this.scramble ^ this;
    (ext.scramble & this)
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

  def l_setAt(at: Int)(vvv: Long): BitMap = {

    //new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
    at match {
      case 0  => new BitMap(vvv, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 1  => new BitMap(u00, vvv, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 2  => new BitMap(u00, u01, vvv, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 3  => new BitMap(u00, u01, u02, vvv, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 4  => new BitMap(u00, u01, u02, u03, vvv, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)

      case 5  => new BitMap(u00, u01, u02, u03, u04, vvv, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 6  => new BitMap(u00, u01, u02, u03, u04, u05, vvv, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 7  => new BitMap(u00, u01, u02, u03, u04, u05, u06, vvv, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 8  => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, vvv, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 9  => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, vvv, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)

      case 10 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, vvv, u11, u12, u13, u14, u15, u16, u17, u18, u19)
      case 11 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, vvv, u12, u13, u14, u15, u16, u17, u18, u19)
      case 12 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, vvv, u13, u14, u15, u16, u17, u18, u19)
      case 13 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, vvv, u14, u15, u16, u17, u18, u19)
      case 14 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, vvv, u15, u16, u17, u18, u19)

      case 15 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, vvv, u16, u17, u18, u19)
      case 16 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, vvv, u17, u18, u19)
      case 17 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, vvv, u18, u19)
      case 18 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, vvv, u19)
      case 19 => new BitMap(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, vvv)
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
    new BitMap(BitMap.umask & ~u00, BitMap.umask & ~u01, BitMap.umask & ~u02, BitMap.umask & ~u03, BitMap.umask & ~u04,
      BitMap.umask & ~u05, BitMap.umask & ~u06, BitMap.umask & ~u07, BitMap.umask & ~u08, BitMap.umask & ~u09,
      BitMap.umask & ~u10, BitMap.umask & ~u11, BitMap.umask & ~u12, BitMap.umask & ~u13, BitMap.umask & ~u14,
      BitMap.umask & ~u15, BitMap.umask & ~u16, BitMap.umask & ~u17, BitMap.umask & ~u18, BitMap.umask & ~u19)

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

    def recLook(h: Int): BitMap = {
      if (h < 0) {
        BitMap.zero
      } else {
        val lat = l_getAt(h)
        if (lat != 0) {
          BitMap.zero.l_setAt(h)(firstInLong(lat))
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

    var res: List[BitMap] = Nil

    var suivi = this

    while (!suivi.isNull) {
      val bit = suivi.firstSetBitBm
      val fill = BitMap.followTrail(bit, suivi)

      res = fill :: res
      suivi = suivi ^ fill

    }
    res

  }

  def &(that: BitMap) = {
    new BitMap(
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

  def |(that: BitMap) = {
    new BitMap(
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

  def ^(that: BitMap) = {
    new BitMap(
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
    new BitMap(0L, u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18)

  }

  def -- = {
    new BitMap(u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, 0L)
  }

  def << = {
    //System.err.println("apply << ");
    new BitMap(u00 >>> 1, u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1, u19 >>> 1)
  }

  def >> = {
    new BitMap(BitMap.umask & u00 << 1, BitMap.umask & u01 << 1, BitMap.umask & u02 << 1, BitMap.umask & u03 << 1, BitMap.umask & u04 << 1, BitMap.umask & u05 << 1, BitMap.umask & u06 << 1, BitMap.umask & u07 << 1, BitMap.umask & u08 << 1, BitMap.umask & u09 << 1, BitMap.umask & u10 << 1, BitMap.umask & u11 << 1, BitMap.umask & u12 << 1, BitMap.umask & u13 << 1, BitMap.umask & u14 << 1, BitMap.umask & u15 << 1, BitMap.umask & u16 << 1, BitMap.umask & u17 << 1, BitMap.umask & u18 << 1, BitMap.umask & u19 << 1)
  }

  def <<- = {
    new BitMap(u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1, u19 >>> 1, 0L)

  }

  def <<+ = {
    new BitMap(0L, u00 >>> 1, u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1)

  }

  def >>- = {
    new BitMap(BitMap.umask & u01 << 1, BitMap.umask & u02 << 1, BitMap.umask & u03 << 1, BitMap.umask & u04 << 1, BitMap.umask & u05 << 1, BitMap.umask & u06 << 1, BitMap.umask & u07 << 1, BitMap.umask & u08 << 1, BitMap.umask & u09 << 1, BitMap.umask & u10 << 1, BitMap.umask & u11 << 1, BitMap.umask & u12 << 1, BitMap.umask & u13 << 1, BitMap.umask & u14 << 1, BitMap.umask & u15 << 1, BitMap.umask & u16 << 1, BitMap.umask & u17 << 1, BitMap.umask & u18 << 1, BitMap.umask & u19 << 1, 0L)
  }

  def >>+ = {
    new BitMap(0L, BitMap.umask & u00 << 1, BitMap.umask & u01 << 1, BitMap.umask & u02 << 1, BitMap.umask & u03 << 1, BitMap.umask & u04 << 1, BitMap.umask & u05 << 1, BitMap.umask & u06 << 1, BitMap.umask & u07 << 1, BitMap.umask & u08 << 1, BitMap.umask & u09 << 1, BitMap.umask & u10 << 1, BitMap.umask & u11 << 1, BitMap.umask & u12 << 1, BitMap.umask & u13 << 1, BitMap.umask & u14 << 1, BitMap.umask & u15 << 1, BitMap.umask & u16 << 1, BitMap.umask & u17 << 1, BitMap.umask & u18 << 1)
  }

  def paintCrossAt(x: Int, y: Int) = {
    var res = l_setAt(y)(-1L)
    for (i <- 0 until 20) {
      res = res.set(x)(i)(1)
    }
    res
  }

  def noyau = {
    var last= ~this    
    var curr=(~this).scramble | BitMap.border;

    
    while(!(~curr).isNull){
      last=curr
      curr=curr.scramble
    }
    
    ~last
  }
  
  def closestPointHere(from : BitMap)={
    var dist=0;
    var curr=from;
    
    val thisNotNull= !this.isNull && !from.isNull
    while(((curr & this).isNull) & (thisNotNull) ){
      curr= curr | curr.scramble
      dist= dist+1
     // Console.err.println("curr\n"+curr);
    }
    
    new Tuple2(dist,curr & this)
  }

}



class servMap {
  val dat = Array.fill[Int](ServWorld.W, ServWorld.H) { -1 }

  
  def extractBm(nbPlayer : Int)={
    var bm0 = BitMap.zero;
    var bm1 = BitMap.zero;
    var bm2 = BitMap.zero;
    var bm3 = BitMap.zero;    
    
    for( i<- 0 until ServWorld.W; j <- 0 until ServWorld.H){
      dat(i)(j) match{
        case 0 => bm0 = bm0.set(i)(j)(1);
        case 1 => bm1 = bm1.set(i)(j)(1);
        case 2 => bm2 = bm2.set(i)(j)(1);
        case 3 => bm3 = bm3.set(i)(j)(1);
        case _ => ()
      }
    }
    
    val res=nbPlayer match{
        case 2 => Array(bm0,bm1)
        case 3 => Array(bm0,bm1,bm2)
        case 4 => Array(bm0,bm1,bm2,bm3)
    }
    
    res
  }
}
class servCoord(
    val x: Int, val y: Int, val back: Int) {

  override def toString = {
    " " + x + " " + y
  }

  
  def dirToCoord(dir : Int)={
      dir match {
        case 0 => new servCoord(x, y - 1, 0);
        case 1 => new servCoord(x + 1, y, 0);
        case 2 => new servCoord(x, y + 1, 0);
        case 3 => new servCoord(x - 1, y, 0);
      }    
  }  
}
abstract class servBot {
  def init(nbPlay: Int, idP: Int)
  def input(coords: Array[servCoord], dat: servMap)
  def turn: servCoord
  def name: String
}

object Player extends App {
  val W: Int = 35
  val H: Int = 20

  def convertMap(sl : Seq[String]) = {
    val res = new servMap
    

    var j=0;
    for(s <- sl){
      var i=0;
      for(c <- s){
        c match{
          case '.' => res.dat(i)(j)= -1;
          case '0' => res.dat(i)(j)= 0;
          case '1' => res.dat(i)(j)= 1;
          case '2' => res.dat(i)(j)= 2;
          case '3' => res.dat(i)(j)= 3;
          
        }
        
        i=i+1
      }
      
      j=j+1;
    }
    res
  }
  
  val myBot: servBot = Const.BOT;
  val opponentcount = readInt // Opponent count

  myBot.init(opponentcount, 0);
  // game loop
  while (true) {
    val gameround = readInt
    // x: Your x position
    // y: Your y position
    // backintimeleft: Remaining back in time
    val someCoord = for (i <- 0 until (opponentcount + 1)) yield {
      // opponentx: X position of the opponent
      // opponenty: Y position of the opponent
      // opponentbackintimeleft: Remaining back in time of the opponent
      val Array(opponentxb, opponentyb, opponentbackintimeleft) = for (i <- readLine split " ") yield i.toInt
      val opponentx = if(opponentxb>0) opponentxb else 0;
      val opponenty = if(opponentyb>0) opponentyb else 0;
      
      new servCoord(opponentx, opponenty, opponentbackintimeleft)

    }

    val ls = for (i <- 0 until 20) yield {
      val line = readLine // One line of the map ('.' = free, '0' = you, otherwise the id of the opponent)
      line
    }
    
          val t0 = System.nanoTime()

    
    val map=convertMap(ls)
    val bms = map.extractBm(someCoord.size)
    
    val sc : List[Int] = for(b <- bms.toList) yield{
      b.countBitset
    }
          
    val maxSc = sc.max
    val diff=maxSc - sc.head
    Console.err.println("max score is "+maxSc+"  to be first is "+diff);
    

    
    myBot.input(someCoord.toArray, map)
    
    val order=myBot.turn
    
      val t1 = System.nanoTime()
      
      var t : Double =( t1 - t0) / 1000
      Console.err.println(""+ myBot.name+" "+t+" nanoseconds "+(t/1000)+" millisecondes");    

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    if(diff > 20 && someCoord.head.back==1){
      println("BACK 25");
    }else
      println("" + order.x + " " + order.y) // action: "x y" to move or "BACK rounds" to go back in time
  }
}


//----------------------------------------------------------------



class Bot001 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    coord = coords(idP)
  }

  override def turn: servCoord = {

    val W = ServWorld.W;
    val H = ServWorld.H;

    def clipx(x: Int) = x match {
      case a if (a < 0)  => 0
      case a if (a >= W) => W - 1
      case _             => x
    }

    def clipy(x: Int) = x match {
      case a if (a < 0)  => 0
      case a if (a >= H) => H - 1
      case _             => x
    }

    val x = rand.nextInt(3) + coord.x - 1;
    val y = rand.nextInt(3) + coord.y - 1;

    val flip = rand.nextBoolean()

    if (flip)
      new servCoord(coord.x, clipy(y), 0);
    else
      new servCoord(clipx(x), coord.y, 0);
  }
  override def name: String = {
    "Default (" + idP + ")";
  }
  
}


class Bot002 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  var coords: Array[servCoord] = null
  var dat: servMap = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    coord = coords(idP)

    this.coords = coords
    this.dat = dat
  }

  override def turn: servCoord = {

    val W = ServWorld.W;
    val H = ServWorld.H;

    val bms = dat.extractBm(coords.size);
    val firstZone = BitMap.firstArea(bms, coords, idP)

    if (!(firstZone.isNull)) {
      val possibi = BitMap.firstDirTo(BitMap.zero.set(coord.x)(coord.y)(1), firstZone)

      //println("\n"+BitMap.zero.set(coord.x)(coord.y)(1));
      //println("\n"+firstZone);
      //println(""+possibi);

      val rx = rand.nextInt(possibi.size)
      val dir = possibi(rx)
      dir match {
        case 0 => new servCoord(coord.x, coord.y - 1, 0);
        case 1 => new servCoord(coord.x + 1, coord.y, 0);
        case 2 => new servCoord(coord.x, coord.y + 1, 0);
        case 3 => new servCoord(coord.x - 1, coord.y, 0);

      }

    } else {
      val v = BitMap.voidArea(bms)

      if (v.isNull) {
        new servCoord(coord.x, coord.y, 0);
      } else {
        val possibi = BitMap.firstDirTo(BitMap.zero.set(coord.x)(coord.y)(1), v)
        val rx = rand.nextInt(possibi.size)
        val dir = possibi(rx)
        dir match {
          case 0 => new servCoord(coord.x, coord.y - 1, 0);
          case 1 => new servCoord(coord.x + 1, coord.y, 0);
          case 2 => new servCoord(coord.x, coord.y + 1, 0);
          case 3 => new servCoord(coord.x - 1, coord.y, 0);

        }
      }

    }

  }
  override def name: String = {
    "Default (" + idP + ")";
  }

}


class Bot003 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  var coords: Array[servCoord] = null
  var dat: servMap = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    coord = coords(idP)

    this.coords = coords
    this.dat = dat
  }

  override def turn: servCoord = {

    val W = ServWorld.W;
    val H = ServWorld.H;

    val bms = dat.extractBm(coords.size);
    val firstZone = BitMap.firstArea(bms, coords, idP)
    
    val void = BitMap.voidArea(bms)
    
    val consolidatedZone = (firstZone | bms(idP))
    val bordFirstExt= ( consolidatedZone).scramble ^ consolidatedZone 
    val rawFront =  (bordFirstExt.scramble & consolidatedZone )
    val consolidateFront = BitMap.closeDiag(rawFront, void)
    
    
    
    
    //val bordFirst = (bordFirstExt.scramble & consolidatedZone ) & (~bms(idP))
    //val bordFirst = (consolidateFront | rawFront) & (~bms(idP))
    
    val trail = BitMap.followTrail(BitMap.zero.set(coord.x)(coord.y)(1), (consolidateFront | rawFront))
    
    val bordFirst = ( rawFront) &  (~bms(idP))

    if (!(bordFirst.isNull)) {
      val possibi = BitMap.firstDirTo(BitMap.zero.set(coord.x)(coord.y)(1), bordFirst)

      //println("\n"+BitMap.zero.set(coord.x)(coord.y)(1));
      //println("\n"+firstZone);
      //println("\n"+(firstZone | bms(idP)));
     // println("rawFront\n"+rawFront);
     // println("consolidateFront\n"+consolidateFront);
     // println("bordFirst\n"+bordFirst);
      
    //   println("trail\n"+trail);
      //println(""+possibi);

      val rx = rand.nextInt(possibi.size)
      val dir = possibi(rx)
      dir match {
        case 0 => new servCoord(coord.x, coord.y - 1, 0);
        case 1 => new servCoord(coord.x + 1, coord.y, 0);
        case 2 => new servCoord(coord.x, coord.y + 1, 0);
        case 3 => new servCoord(coord.x - 1, coord.y, 0);

      }

    } else {
      val v = BitMap.voidArea(bms)

      if (v.isNull) {
        new servCoord(coord.x, coord.y, 0);
      } else {
        val possibi = BitMap.firstDirTo(BitMap.zero.set(coord.x)(coord.y)(1), v)
        val rx = rand.nextInt(possibi.size)
        val dir = possibi(rx)
        dir match {
          case 0 => new servCoord(coord.x, coord.y - 1, 0);
          case 1 => new servCoord(coord.x + 1, coord.y, 0);
          case 2 => new servCoord(coord.x, coord.y + 1, 0);
          case 3 => new servCoord(coord.x - 1, coord.y, 0);

        }
      }

    }

  }
  override def name: String = {
    "Bot003 (" + idP + ")";
  }

}


class Bot004 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  var coords: Array[servCoord] = null
  var dat: servMap = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    coord = coords(idP)

    this.coords = coords
    this.dat = dat
  }
  
  def goTarget(pos : servCoord,targs : List[BitMap], void : BitMap) : servCoord ={
    
    if(targs.isEmpty){
      pos
    }else
    if(!targs.head.isNull){
        val currMap=BitMap.zero.set(pos.x)(pos.y)(1)
         Console.err.println("currMap\n"+currMap);
        Console.err.println("dest\n"+targs.head);
        //Console.err.println("void\n"+void);
          val possibi = BitMap.firstDirToThrough(currMap, targs.head,void)
          Console.err.println("possibi Through void "+possibi);
          if(possibi.nonEmpty){
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)
          }else{
              val possibi = BitMap.firstDirTo(BitMap.zero.set(pos.x)(pos.y)(1), targs.head)
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)            
          }
          
    }else{
      goTarget(pos,targs.tail,void)
    }

  }
  
  def stratBase ={
    
    
        val bms = dat.extractBm(coords.size);
        val terri= bms(idP)
        val void = BitMap.voidArea(bms)
        val firstZone = BitMap.firstArea(bms, coords, idP)
        
        val terriDiag=BitMap.closeDiag(terri, void)
        
        Console.err.println("terriDiag\n"+terriDiag);
        
        val bordFirst = (( firstZone | terri).frontierMap) &  (~terri)
        
        goTarget(coord, List(terriDiag,bordFirst,void), void)
        
  }

  override def turn: servCoord = {
    stratBase

  }
  override def name: String = {
    "Bot004 (" + idP + ")";
  }

}

class Bot005 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  var coords: Array[servCoord] = null
  var dat: servMap = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    coord = coords(idP)

    this.coords = coords
    this.dat = dat
  }
  
  def goTarget(pos : servCoord,targs : List[BitMap], void : BitMap) : servCoord ={
    
    if(targs.isEmpty){
      pos
    }else
    if(!targs.head.isNull){
        val currMap=BitMap.zero.set(pos.x)(pos.y)(1)
         //Console.err.println("currMap\n"+currMap);
       // Console.err.println("dest\n"+targs.head);
        //Console.err.println("void\n"+void);
          val possibi = BitMap.firstDirToThrough(currMap, targs.head,void)
         // Console.err.println("possibi Through void "+possibi);
          if(possibi.nonEmpty){
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)
          }else{
              val possibi = BitMap.firstDirTo(BitMap.zero.set(pos.x)(pos.y)(1), targs.head)
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)            
          }
          
    }else{
      goTarget(pos,targs.tail,void)
    }

  }
  
  def stratBase ={
    
    
        val bms = dat.extractBm(coords.size);
        val terri= bms(idP)
        val void = BitMap.voidArea(bms)
        val firstZone = BitMap.firstArea(bms, coords, idP)
        
        val terriDiag=BitMap.closeDiag(terri, void)
        
        val captIfBord=BitMap.enclosed(terri | BitMap.border, void) & (~BitMap.border)
        val prioBord=captIfBord.scramble & BitMap.border
        
        //Console.err.println("prioBord\n"+prioBord);
        
        val bordFirst = (( firstZone | terri).frontierMap) &  (~terri)
        
        goTarget(coord, List(prioBord,terriDiag,bordFirst,void), void)
        
  }

  override def turn: servCoord = {
    stratBase

  }
  override def name: String = {
    "Bot005 (" + idP + ")";
  }

}

class Bot006 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  var coords: Array[servCoord] = null
  var dat: servMap = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    coord = coords(idP)

    this.coords = coords
    this.dat = dat
  }
  
  def goTarget(pos : servCoord,targs : List[BitMap], void : BitMap) : servCoord ={
    
    if(targs.isEmpty){
      pos
    }else
    if(!targs.head.isNull){
        val currMap=BitMap.zero.set(pos.x)(pos.y)(1)
         //Console.err.println("currMap\n"+currMap);
        //Console.err.println("dest\n"+targs.head);
        //Console.err.println("void\n"+void);
          val possibi = BitMap.firstDirToThrough(currMap, targs.head,void)
         // Console.err.println("possibi Through void "+possibi);
          if(possibi.nonEmpty){
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)
          }else{
              val possibi = BitMap.firstDirTo(BitMap.zero.set(pos.x)(pos.y)(1), targs.head)
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)            
          }
          
    }else{
      goTarget(pos,targs.tail,void)
    }

  }
  
  def stratBase ={
    
      def maxOrEmpty(y : List[BitMap])={
        if(y.isEmpty) BitMap.zero else
        {
          y.maxBy { x => x.countBitset }
        }
      }
      
      def extractMax(x : BitMap)={
        maxOrEmpty(x.extractZones)
      }
    
    
        val bms = dat.extractBm(coords.size);
        val terri= bms(idP)
        val void = BitMap.voidArea(bms)
        val firstZone = BitMap.firstArea(bms, coords, idP)
        
        val maxFirstZone= extractMax(firstZone&void)
     //   Console.err.println("maxFirstZone\n"+maxFirstZone);
        
        val maxEmpty = extractMax(void)
      //  Console.err.println("maxEmpty\n"+maxEmpty);
        
        val terriDiag=BitMap.closeDiag(terri, void)
        
        val cross=BitMap.zero.paintCrossAt(coord.x, coord.y) & void // important : uniquement les cases vides vieux !!
        val captIfCross=extractMax(BitMap.enclosed((terri | cross), void  & (~cross)) & firstZone)
       // Console.err.println("captIfCross\n"+captIfCross);
        
        val trig = coords.size match {
          case 2 => 25
          case 3 => 14
          case 4 => 9
          
        }
        
        val prioCross=if(captIfCross.countBitset >=trig) (captIfCross.scramble^captIfCross ) & (~terri ) else BitMap.zero
        
       // Console.err.println("prioCross\n"+prioCross);
        
        
        val bordFirst = (( maxFirstZone | terri).frontierMap) &  (~terri)        
        
        goTarget(coord, List(prioCross,bordFirst,void), void)
        
  }

  override def turn: servCoord = {
    stratBase

  }
  override def name: String = {
    "Bot006 (" + idP + ")";
  }

}

class Bot007Defender extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  var coords: Array[servCoord] = null
  var dat: servMap = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    coord = coords(idP)

    this.coords = coords
    this.dat = dat
  }
  
  def goTarget(pos : servCoord,targs : List[BitMap], void : BitMap) : servCoord ={
    
    if(targs.isEmpty){
      pos
    }else
    if(!targs.head.isNull){
        val currMap=BitMap.zero.set(pos.x)(pos.y)(1)
         //Console.err.println("currMap\n"+currMap);
        //Console.err.println("dest\n"+targs.head);
        //Console.err.println("void\n"+void);
          val possibi = BitMap.firstDirToThrough(currMap, targs.head,void)
         // Console.err.println("possibi Through void "+possibi);
          if(possibi.nonEmpty){
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)
          }else{
              val possibi = BitMap.firstDirTo(BitMap.zero.set(pos.x)(pos.y)(1), targs.head)
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)            
          }
          
    }else{
      goTarget(pos,targs.tail,void)
    }

  }
  
  def stratBase ={
    
        val maxFirstLim = coords.size match {
          case 2 => 9
          case 3 => 9
          case 4 => 9
          
        }        
        
        val trig = coords.size match {
          case 2 => 25
          case 3 => 14
          case 4 => 14
          
        }        
            
    
      def maxOrEmpty(y : List[BitMap])={
        if(y.isEmpty) BitMap.zero else
        {
          y.maxBy { x => x.countBitset }
        }
      }
      
      def extractMax(x : BitMap,minCount : Int)={
        val map=maxOrEmpty(x.extractZones)
        if(map.countBitset < minCount) BitMap.zero else map
      }
    
    
        val bms = dat.extractBm(coords.size);
        val terri= bms(idP)
        val void = BitMap.voidArea(bms)
        val firstZone = BitMap.rawFirstMap( coords, idP)
        
        val playerPosMap=coords.map( x=> BitMap.zero.set(x.x)(x.y)(1))
        val me = playerPosMap(idP)
        val ePoint=playerPosMap.foldLeft(BitMap.zero){_ | _}^me
        //Console.err.println("firstZone\n"+firstZone);
        //Console.err.println("firstZone noyau\n"+firstZone.noyau);
        
        val maxFirstZone= extractMax(firstZone&(void |terri),maxFirstLim)
        //Console.err.println("maxFirstZone\n"+maxFirstZone);
        
        
         //Console.err.println("ePoint\n"+ePoint);
        val (dist,clothestTo)=(maxFirstZone & void).closestPointHere(ePoint)
        //Console.err.println("clothestTo\n"+clothestTo);
        
        //val noyau= (firstZone );

        val maxEmpty = extractMax(void,0)
      //  Console.err.println("maxEmpty\n"+maxEmpty);
        
        
        //val cross=BitMap.zero.paintCrossAt(coord.x, coord.y) & void // important : uniquement les cases vides vieux !!
       // val captIfCross=extractMax(BitMap.enclosed((terri | cross), void  & (~cross)) & firstZone , trig)
       // Console.err.println("captIfCross\n"+captIfCross);
                     
        
        val bordFirstZone = (( maxFirstZone | terri).frontierMap ) &  (~terri)      
        //Console.err.println("bordFirst\n"+bordFirstZone);
        
        goTarget(coord, List(clothestTo,bordFirstZone,void), void)
        
  }

  override def turn: servCoord = {
    stratBase

  }
  override def name: String = {
    "Bot007Defender (" + idP + ")";
  }

}

object Const{
  val BOT = new Bot007Defender()
  
}
