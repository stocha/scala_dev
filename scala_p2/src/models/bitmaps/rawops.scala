package models.bitmaps

/**
 * @author Jahan
 */

object rawfixbm {

  final val rH: Int = 20
  final val rW: Int = 35

  final val rawzero = {
    new rdatbm(
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

  final private val umask = {
    (-1L) >>> (64 - rW);
  }

  final class rdatbm(
      final val u00: Long,
      final val u01: Long,
      final val u02: Long,
      final val u03: Long,
      final val u04: Long,

      final val u05: Long,
      final val u06: Long,
      final val u07: Long,
      final val u08: Long,
      final val u09: Long,

      final val u10: Long,
      final val u11: Long,
      final val u12: Long,
      final val u13: Long,
      final val u14: Long,

      final val u15: Long,
      final val u16: Long,
      final val u17: Long,
      final val u18: Long,
      final val u19: Long //,       
      //val end : Long
      ) {

    final def l_getAt(at: Int): Long = {
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

    final def l_setAt(at: Int)(vvv: Long): rdatbm = {

      //new BMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      at match {
        case 0  => new rdatbm(vvv, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 1  => new rdatbm(u00, vvv, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 2  => new rdatbm(u00, u01, vvv, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 3  => new rdatbm(u00, u01, u02, vvv, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 4  => new rdatbm(u00, u01, u02, u03, vvv, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)

        case 5  => new rdatbm(u00, u01, u02, u03, u04, vvv, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 6  => new rdatbm(u00, u01, u02, u03, u04, u05, vvv, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 7  => new rdatbm(u00, u01, u02, u03, u04, u05, u06, vvv, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 8  => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, vvv, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 9  => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, vvv, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19)

        case 10 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, vvv, u11, u12, u13, u14, u15, u16, u17, u18, u19)
        case 11 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, vvv, u12, u13, u14, u15, u16, u17, u18, u19)
        case 12 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, vvv, u13, u14, u15, u16, u17, u18, u19)
        case 13 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, vvv, u14, u15, u16, u17, u18, u19)
        case 14 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, vvv, u15, u16, u17, u18, u19)

        case 15 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, vvv, u16, u17, u18, u19)
        case 16 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, vvv, u17, u18, u19)
        case 17 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, vvv, u18, u19)
        case 18 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, vvv, u19)
        case 19 => new rdatbm(u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, vvv)
      }
    }

    final def isNull = {
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

    final def notNull = {
      !(isNull)
    }

    final def get(x: Int)(y: Int): Long = {
      val r = l_getAt(y)
      (r >>> x) & 1L
    }

    final def set(x: Int)(y: Int)(v: Long) = {
      val r: Long = l_getAt(y)
      val b: Long = (1L << x)
      val nb: Long = (~b & r) | ((v << x) & b)
      //println("old "+llong_toString(r));
      //println("new "+llong_toString(nb));
      val res = l_setAt(y)(nb)
      //println("res is \n"+res);
      res
    }

    final def unary_~ = {
      new rdatbm(rawfixbm.umask & ~u00, rawfixbm.umask & ~u01, rawfixbm.umask & ~u02, rawfixbm.umask & ~u03, rawfixbm.umask & ~u04,
        rawfixbm.umask & ~u05, rawfixbm.umask & ~u06, rawfixbm.umask & ~u07, rawfixbm.umask & ~u08, rawfixbm.umask & ~u09,
        rawfixbm.umask & ~u10, rawfixbm.umask & ~u11, rawfixbm.umask & ~u12, rawfixbm.umask & ~u13, rawfixbm.umask & ~u14,
        rawfixbm.umask & ~u15, rawfixbm.umask & ~u16, rawfixbm.umask & ~u17, rawfixbm.umask & ~u18, rawfixbm.umask & ~u19)

    }

    final def not = {
      ~this
    }

    final def &(that: rdatbm) = {
      new rdatbm(
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

    final def |(that: rdatbm) = {
      new rdatbm(
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

    final def ^(that: rdatbm) = {
      new rdatbm(
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

    final def ++ = {
      new rdatbm(0L, u00, u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18)

    }

    final def -- = {
      new rdatbm(u01, u02, u03, u04, u05, u06, u07, u08, u09, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, 0L)
    }

    final def << = {
      //System.err.println("apply << ");
      new rdatbm(u00 >>> 1, u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1, u19 >>> 1)
    }

    final def >> = {
      new rdatbm(umask & u00 << 1, umask & u01 << 1, umask & u02 << 1, umask & u03 << 1, umask & u04 << 1, umask & u05 << 1, umask & u06 << 1, umask & u07 << 1, umask & u08 << 1, umask & u09 << 1, umask & u10 << 1, umask & u11 << 1, umask & u12 << 1, umask & u13 << 1, umask & u14 << 1, umask & u15 << 1, umask & u16 << 1, umask & u17 << 1, umask & u18 << 1, umask & u19 << 1)
    }

    final def <<- = {
      new rdatbm(u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1, u19 >>> 1, 0L)

    }

    def <<+ = {
      new rdatbm(0L, u00 >>> 1, u01 >>> 1, u02 >>> 1, u03 >>> 1, u04 >>> 1, u05 >>> 1, u06 >>> 1, u07 >>> 1, u08 >>> 1, u09 >>> 1, u10 >>> 1, u11 >>> 1, u12 >>> 1, u13 >>> 1, u14 >>> 1, u15 >>> 1, u16 >>> 1, u17 >>> 1, u18 >>> 1)

    }

    def >>- = {
      new rdatbm(umask & u01 << 1, umask & u02 << 1, umask & u03 << 1, umask & u04 << 1, umask & u05 << 1, umask & u06 << 1, umask & u07 << 1, umask & u08 << 1, umask & u09 << 1, umask & u10 << 1, umask & u11 << 1, umask & u12 << 1, umask & u13 << 1, umask & u14 << 1, umask & u15 << 1, umask & u16 << 1, umask & u17 << 1, umask & u18 << 1, umask & u19 << 1, 0L)
    }

    def >>+ = {
      new rdatbm(0L, umask & u00 << 1, umask & u01 << 1, umask & u02 << 1, umask & u03 << 1, umask & u04 << 1, umask & u05 << 1, umask & u06 << 1, umask & u07 << 1, umask & u08 << 1, umask & u09 << 1, umask & u10 << 1, umask & u11 << 1, umask & u12 << 1, umask & u13 << 1, umask & u14 << 1, umask & u15 << 1, umask & u16 << 1, umask & u17 << 1, umask & u18 << 1)
    }

    def ==(that: rdatbm) = {
      (this ^ that).isNull
    }

  } // rdatbm   
}

object Bm {
  import rawfixbm._

  final val zero = new Bm(rawzero)
  
  final val H = rH
  final val W = rW

  final class Bm( final val b: rdatbm) {
    final def &(that: Bm) = new Bm(b & that.b)
    final def ^(that: Bm) = new Bm(b ^ that.b)
    final def |(that: Bm) = new Bm(b | that.b)

    final def ++ = new Bm(b++)
    final def -- = new Bm(b--)
    final def << = new Bm(b<<)
    final def >> = new Bm(b>>)
    final def >>+ = new Bm(b>>+)
    final def >>- = new Bm(b>>-)
    final def <<+ = new Bm(b<<+)
    final def <<- = new Bm(b<<-)
      
    final def get(x: Int)(y: Int) = b.get(x)(y)

    final def set(x: Int)(y: Int)(v: Long) = new Bm(b.set(x)(y)(v))

    final def isNull = b.isNull

    //----------------------------------------------------

    override def toString() = {
      var res = "\n";

      for (j <- 0 until rH) {
        for (i <- 0 until rW) {
          val v = get(i)(j)
          val c = if (v==1) '#' else '-'          
          res+=" "+c
        }
        res+="\n"          
      }
      res
    }
      
    final def rule30 ={
      val x : Bm.Bm = this
      ((x>>) ^ (x | (x<<)) ++ )| x 
    }      

    final def shiftIn(dir: Int) = {
      dir match {
        case 0 => this--
        case 1 => this>>
        case 2 => this++
        case 3 => this<<
      }
    }

    final def closestPointHere(from: Bm): Tuple2[Int, Bm] = {
      var dist = 0;
      var curr = from;

      val thisNotNull = (!this.isNull) && (!from.isNull)
      while (((curr & this).isNull) & (thisNotNull)) {
        curr = curr | curr.scramble
        dist = dist + 1
      }

      new Tuple2(dist, curr & this)
    }

    final def ==(that: Bm) = {
      (this ^ that).isNull
    }

    final def scramble = {
      var check = this;
      val oldcheck = check;

      check = (check | (oldcheck>>))
      check = (check | (oldcheck<<))
      check = (check | (oldcheck--))
      check = (check | (oldcheck++))

      check
    }

    final def angularScramble = {
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

    final def scrUL = {
      var check = this;
      val oldcheck = check;

      check = (check | (oldcheck<<-))
      check = (check | (oldcheck<<))
      check = (check | (oldcheck--))

      check
    }

    final def scrUR = {
      var check = this;
      val oldcheck = check;

      check = (check | (oldcheck>>-))
      check = (check | (oldcheck>>))
      check = (check | (oldcheck--))

      check
    }

    final def scrDL = {
      var check = this;
      val oldcheck = check;

      check = (check | (oldcheck<<+))
      check = (check | (oldcheck<<))
      check = (check | (oldcheck++))

      check
    }

    final def scrDR = {
      var check = this;
      val oldcheck = check;

      check = (check | (oldcheck>>+))
      check = (check | (oldcheck>>))
      check = (check | (oldcheck++))

      check
    }

    final def apply(x: Int)(y: Int) = {
      b.get(x)(y)
    }
  } // Bm class

} // Bm object

object sBitStack {

  final def apply(sz: Int): sBitStack = {
    def mk(curr: List[Bm.Bm], sz: Int): List[Bm.Bm] = {
      if (sz == 0) {
        curr
      } else {
        Bm.zero :: curr
      }
    }

    new sBitStack(mk(List(), sz))
  }

  final class sBitStack( final val dat: List[Bm.Bm]) {
    def >> = {
      new sBitStack(
        dat.map { x => x>> })
    }
    def << = {
      new sBitStack(
        dat.map { x => x<< })
    }
    def ++ = {
      new sBitStack(
        dat.map { x => x++ })
    }
    def -- = {
      new sBitStack(
        dat.map { x => x-- })
    }

    def mask(m: Bm.Bm) = {
      new sBitStack(
        dat.map { x => x & m })
    }
  }
}

object rawops extends App {
  override def main(args: Array[String]) {
    println("Hello BitMaps free")    
    
    var b=Bm.zero.set(Bm.W/2)(0)(1)
    
    
    for(i<-0 until Bm.H){

      println( ""+b)
            b=b.rule30
    }
    
  }
}