import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/

 
 
object Player extends App {
val W : Int =35
val H : Int =20    
    
    val opponentcount = readInt // Opponent count
    val rand= new Random(0xFF9844)
    // game loop
    while(true) {
        val gameround = readInt
        // x: Your x position
        // y: Your y position
        // backintimeleft: Remaining back in time
        val Array(x, y, backintimeleft) = for(i <- readLine split " ") yield i.toInt
        for(i <- 0 until opponentcount) {
            // opponentx: X position of the opponent
            // opponenty: Y position of the opponent
            // opponentbackintimeleft: Remaining back in time of the opponent
            val Array(opponentx, opponenty, opponentbackintimeleft) = for(i <- readLine split " ") yield i.toInt
        }
        for(i <- 0 until 20) {
            val line = readLine // One line of the map ('.' = free, '0' = you, otherwise the id of the opponent)
        }
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        val xoutr=rand.nextInt(3)+x-1;
        val youtr=rand.nextInt(3)+y-1;
        val xout=xoutr match{
            case a if (a < 0) => 0
            case a if (a >= W) => W-1
        }
        
        val yout=xoutr match{
            case a if (a < 0) => 0
            case a if (a >= H) => H-1
        }        
        
        println(""+xout+" "+yout) // action: "x y" to move or "BACK rounds" to go back in time
    }
}