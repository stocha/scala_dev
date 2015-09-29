package v2


/**
 * @author Jahan
 */

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