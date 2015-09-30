package v2

/**
 * @author Jahan
 */
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
      //System.err.println("backing "+logMove);
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