package v2

/**
 * @author Jahan
 */

class log{
  var d : Array[Int] = Array()
  var init : Long =0
  
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
      }else{
        
      }
      
      b.genMove(ref)

    }  
  
}


class bv_followTrail(var  dst :BMap ) ( choicePriority : (Int)=>Int)
extends agentAbstract{
  
    def genMove (ref : GameState4P) ={
      val bv = new BotVocabulary(ref)
      
      dst= dst & (~bv.me)
      
      val dir =bv.goTo(dst)
      if(dir.size>=1) dir.maxBy(choicePriority) else 4
    }
  
}