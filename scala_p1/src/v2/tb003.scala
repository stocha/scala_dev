package v2

/**
 * @author Jahan
 *
 *
 *
 */

class tb003 extends agentAbstract {

  var currPlan: agentAbstract = null

  val stopr: ((GameState4P, BMap) => Boolean) = { (x, y) => !((y & (x.tr.void | x.pos.pos0)) == y) }

  def plansTrailTry(p: Array[BMap]) = {
    val sorted = p

    sorted.map { pp => new Tuple2(pp, new bv_trailStop(pp)(stopr)(x => x)) }

  }

  def tryPlansList(p: List[Tuple2[BMap, agentAbstract]], bv: BotVocabulary): Int = {
    if (p.isEmpty) -1 else {
      bv.forsee_with(new bv_tronRacer, p.head._2)((x: GameState4P) => ((p.head._1 & x.tr.pos0) == p.head._1))(p.size)((x: GameState4P) => false)(tryPlansList(p.tail, bv))
    }

  }

  def genBmSquare(p: Seq[Tuple2[Int, Int]], bv: BotVocabulary) = {
    for (Tuple2(x, y) <- p) yield {
      bv.border(bv.squareInDir(x, y) & bv.st.tr.void)
    }
  }

  def doPlan(ref: GameState4P) = {
    val bv = new BotVocabulary(ref)

    val sq0: BMap = bv.border(bv.squareInDir(0, 5))
    //Console.err.println(""+sq0);
    val plan0 = new bv_trailStop(sq0)(stopr)(x => x)

    //val bmToTry=Array(bv.border( bv.squareInDir(0, 5) ),bv.border( bv.squareInDir(2, 5) ),bv.border( bv.squareInDir(2, 3) )).sortBy { x => -x.countBitset }

    val tmpF0 = for (d <- 0 until 4; s <- 4 to 14) yield {
      (new Tuple2(d, s))
    }
    val bmToTry = (genBmSquare(tmpF0, bv).sortBy { y => -(y.countBitset) }).filter { x => x.countBitset>0 }
    val idPlan = tryPlansList(plansTrailTry(bmToTry.toArray).toList, bv)

    // System.err.println("idPlan = "+idPlan);
    // System.err.println(""+bmToTry.reverse( idPlan -1));

    if (idPlan != -1) {
      currPlan = new bv_trailStop(bmToTry.reverse(idPlan - 1))(stopr)(x => x)
      currPlan.genMove(ref)
    } else {
      val props = (bv.firstZoneHeuristic & ref.tr.void).split

      if (props.size == 0) {
        val props = ( ref.tr.void).split

        if (props.size == 0) {
          4
        } else {

          val dest = bv.border((props.sortBy { x => -x.countBitset }).head)
      //    System.err.println("dest = \n" + dest);

          currPlan = new bv_trailStop(dest)(stopr)(x => x)
          currPlan.genMove(ref)
        }
      } else {

        val dest = bv.border((props.sortBy { x => -x.countBitset }).head)
       // System.err.println("dest = \n" + dest);

        currPlan = new bv_trailStop(dest)(stopr)(x => x)
        currPlan.genMove(ref)
      }

    }

  }

  var logMove: log = new log
  override def backMove() {
    //System.err.println("backing "+logMove);
    logMove.undo()
  }

  def genMove(ref: GameState4P) = {
    logMove.blockControl {

      if (currPlan == null) {
        doPlan(ref)
      } else {
        val m = currPlan.genMove(ref)
        if (m != 4) m else {
          currPlan = null
          doPlan(ref)

        }
      }

    }

  }

}