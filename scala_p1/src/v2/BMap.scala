package v2

import scala.reflect.macros.FrontEnds

/**
 * @author Jahan
 */

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

  def fromString(lines: List[String]): BMap = {
    var res = BMap.zero

    //  System.err.println("" + lines);

    for (j <- 0 until 20) {
      //   System.err.println("" + lines(j));

      val nosp = lines(j).replaceAll(" ", "")
      for (i <- 0 until 35) {

        val c = nosp.charAt(i)
        val s = c match {
          case '#' => 1L
          case _   => 0L
        }
        res = res.set(i)(j)(s)
      }
    }

    res
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

  val alternatedBM = {
    var r = BMap.zero
    for (i <- 0 until 35; j <- 0 until 20) {
      r = r.set(i)(j)(i & 1L);
    }
    r
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

    ((ul | ur | dl | dr) & void) | frontier
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

    //    Console.err.println("begin search to\n"+goal );
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
    //System.err.println("full"+full);
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

  def notNull = {
    !(isNull)
  }

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

  def closeDiag = {
    val frontier = this
    val void = ~this

    val ul = ((frontier--) & (frontier<<)) & ~(frontier<<-)
    val ur = ((frontier--) & (frontier>>)) & ~(frontier>>-)
    val dl = ((frontier++) & (frontier<<)) & ~(frontier<<+)
    val dr = ((frontier++) & (frontier>>)) & ~(frontier>>+)

    ((ul | ur | dl | dr) & void) | frontier
  }

  def border = {
    val ext = this.scramble ^ this;
    (ext.angularScramble & this) | (this & BMap.border)
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
    // throw new RuntimeException("toto");
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
    var res = "\n";

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

  def shiftIn(dir: Int) = {
    dir match {
      case 0 => this--
      case 1 => this>>
      case 2 => this++
      case 3 => this<<
    }
  }

  def ==(that: BMap) = {
    (this ^ that).isNull
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

  def shadow(v: BMap, dir: Int) = {

    // System.err.println("shad\n"+this+" \n"+v);
    def doCalc(op: BMap => BMap) = {

      var last = this
      var curr = (op(this)) & v;

      while ((!(last ^ curr).isNull) && (curr & ~v).isNull) {

        last = curr
        curr = (op(curr) | curr)
        //    System.err.println("last\n"+last+" \n"+curr+" currNV\n"+(curr & v)+" curXorLast\n "+(last ^ curr));
      }

      last

    }

    dir match {
      case 0 => doCalc(x => x--)
      case 1 => doCalc(x => x>>)
      case 2 => doCalc(x => x++)
      case 3 => doCalc(x => x<<)

    }

  }

  def closestPointHere(from: BMap): Tuple2[Int, BMap] = {
    var dist = 0;
    var curr = from;
    //Console.err.println("curr\n"+curr+" \n"+dist);

    val thisNotNull = (!this.isNull) && (!from.isNull)
    while (((curr & this).isNull) & (thisNotNull)) {
      curr = curr | curr.scramble
      dist = dist + 1
      // Console.err.println("curr\n"+curr+" \n"+dist);
    }

    new Tuple2(dist, curr & this)
  }

  def scaleToSize(v: BMap, sz: Int): BMap = {

    val b = this
    def up(it: BMap): BMap = {
      if (it.countBitset >= sz) it else {
        val sc = it.scramble & v
        if (sc == it) sc else
          up(sc)

      }
    }

    def down(it: BMap): BMap = {
      if (it.countBitset < sz) it else {
        val sc = it & ((~it).scramble)
        if (sc == it) sc else
          down(sc)

      }
    }

    if (b.countBitset <= sz) up(b) else up(down(b))
  }

}

object bitStack {

  def apply() = {
    new bitStack(
      BMap.zero, BMap.zero,
      BMap.zero, BMap.zero,
      BMap.zero, BMap.zero,
      BMap.zero, BMap.zero)
  }

  def sumFromTo(from: BMap, to: BMap, target: BMap): bitStack = {
    var dist = 0;
    var curr = to;

    var res = bitStack()
    //Console.err.println("curr\n"+curr+" \n"+dist);

    //Console.err.println("way\n"+(from | to));

    val thisNotNull = (!from.isNull) && (!to.isNull)
    var last = BMap.zero
    while (((curr & from).isNull) & (thisNotNull)) {
      last = curr;
      curr = curr | curr.scramble
      // Console.err.println("curr\n"+curr+" \n"+dist);

      val u = (res--)
      val r = (res>>)
      val d = (res++)
      val l = (res<<)
      
      val fr = (curr ^ last)

      res = res max u.mask(fr)
      res = res max r.mask(fr)
      res = res max d.mask(fr)
      res = res max l.mask(fr)

      res = res.add(fr & target)
      dist = dist + 1
      //Console.err.println("res" + res + " \n" + dist);
    }

    res
  }

}
class bitStack(
    val a: BMap,
    val b: BMap,
    val c: BMap,
    val d: BMap,
    val e: BMap,
    val f: BMap,
    val g: BMap,
    val h: BMap) {

  def apply(i: Int)(j: Int) = {
    var acc = 0L
    acc |= h(i)(j)
    acc = acc << 1
    acc |= g(i)(j)
    acc = acc << 1
    acc |= f(i)(j)
    acc = acc << 1
    acc |= e(i)(j)

    acc = acc << 1
    acc |= d(i)(j)
    acc = acc << 1
    acc |= c(i)(j)
    acc = acc << 1
    acc |= b(i)(j)
    acc = acc << 1
    acc |= a(i)(j)

    acc
  }

  private def carry(x: BMap, y: BMap, c: BMap) = {
    (x & y) | (x & c) | (y & c)
  }

  private def summ(x: BMap, y: BMap, c: BMap) = {
    x ^ y ^ c
  }

  def max(x: bitStack) = {
    var deci = BMap.zero
    var sup = BMap.zero

    def supdo(y: BMap, z: BMap) = {
      sup = (sup & deci) | (~deci & (y & ~(z)))
      deci = deci | (y ^ z)

      (deci & sup & y) | (deci & ~sup & z) | (~deci & y)
    }

    // System.err.println("deci "+deci);
    //  System.err.println("sup "+sup);
    val rh = supdo(h, x.h)
    //  System.err.println("deci "+deci);
    //  System.err.println("sup "+sup);    
    val rg = supdo(g, x.g)
    //  System.err.println("deci "+deci);
    //  System.err.println("sup "+sup);     
    val rf = supdo(f, x.f)
    val re = supdo(e, x.e)

    val rd = supdo(d, x.d)
    val rc = supdo(c, x.c)
    val rb = supdo(b, x.b)
    val ra = supdo(a, x.a)

    new bitStack(
      ra,
      rb,
      rc,
      rd,

      re,
      rf,
      rg,
      rh) {

    }

    /*   val ze=new bitStack(
        rh,
         rg,
          BMap.zero,
           BMap.zero,
            BMap.zero,
             BMap.zero,
              BMap.zero,
               BMap.zero
        )
    ze*/
  }

  def add(x: BMap) = {
    var carr = x

    def spart(x: BMap) = {
      val r = x ^ carr
      carr = x & carr
      r
    }

    new bitStack(
      spart(a),
      spart(b),
      spart(c),
      spart(d),

      spart(e),
      spart(f),
      spart(g),
      spart(h))
  }

  def >> = {
    new bitStack(
      a>>,
      b>>,
      c>>,
      d>>,

      e>>,
      f>>,
      g>>,
      h>>)
  }

  def << = {
    new bitStack(
      a<<,
      b<<,
      c<<,
      d<<,

      e<<,
      f<<,
      g<<,
      h<<)
  }

  def ++ = {
    new bitStack(
      a++,
      b++,
      c++,
      d++,

      e++,
      f++,
      g++,
      h++)
  }

  def -- = {
    new bitStack(
      a--,
      b--,
      c--,
      d--,

      e--,
      f--,
      g--,
      h--)
  }

  def mask(m: BMap) = {
    new bitStack(
      a & m,
      b & m,
      c & m,
      d & m,

      e & m,
      f & m,
      g & m,
      h & m)

  }

  override def toString = {
    var res = "\n"
    for (j <- 0 until 20) {
      for (i <- 0 until 35) {
        val v = apply(i)(j)
        
        if(v!=0)
        res = res + "|" + "%03x".format(apply(i)(j))else
          res = res + "|" +"---"
      }
      res = res + "\n"
    }
    res
  }

}

