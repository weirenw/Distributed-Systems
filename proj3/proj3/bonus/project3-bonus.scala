import java.util.Random
import scala.util.control 
import akka.actor.Actor
import akka.actor._
import akka.actor.{ActorRef,Props,Actor,ActorSystem}; 
import scala.math._ 
object Xnull { 
    var value: Int = -1 
  } 
object Idrange { 
    var v: Int = Int.MaxValue
  } 
/* 
class NodeID(val abits: Int){ 

  var b: Int = 4
  var n: Int = 8
  var bits: Int = abits 
  def prefix(key: NodeID):Int = { 
    var x: Int = bits^key.bits 
    var count: Int = 0; 
    while((x>>>count)!=0){ 
      count+=1
    }
    count=(count+b-1)/b   
    return n-count 
  } 

  def sameid(that: NodeID): Boolean = this.bits==that.bits 
  def islarge(that: NodeID): Boolean = 
      if (this.bits > that.bits) return true 
      else return false 
  def equal(that: NodeID): Boolean = 
      if(this.bits == that.bits) return true 
      else return false 
  def digit(x: Int): Int = { 
      return bits & (15 << (7-x))
 } 
}
 
object NodeID { 
  val rand = new Random 
  def randomID: NodeID = new NodeID(rand.nextInt()) 
}
*/


class PastryNode extends Actor { 
  var uuid: Int = 0
  val (b,l) = (2,8) 
  val (rows, cols) = (32/b, 1 << b) 
/*
  def digit(x: Int): Int = { 
    return uuid&(1 << (1 << b - x))
  } 
*/ 
  def prefix(auuid: Int ,xuuid: Int):Int = { 
    var x: Int = auuid^xuuid 
    var count: Int = 0; 
    while((x >>> count)!=0){ 
      count+=1
    }
    count=(count+b-1)/b   
    return (32/b)-count 
  }

//  def at(r: Int, c: Int) = routingTable( r * cols + c ) 
  var routingTable: Array[Int] = Array.fill((rows+1) * cols)(Xnull.value)
//  val smaller , larger: Array[Int] = Array.fill(1 / 2)(Xnull.value) 
  var leafSet: Array[Int] = Array.fill(2*cols)(Xnull.value) 
/*
  def leafSet_ (ls : Array[Int]) = { 
    ls.filter(_ < this.uuid)copyToArray(smaller) 
    ls.filterNot(_ < this.uuid).copyToArray(larger) 
  }
*/

  def insertLeafSet(newuuid: Int) = {
    val idNill = leafSet.indexOf(Xnull.value) 
    if(idNill>=0){ 
      leafSet(idNill) = newuuid 
//      println("newuuid:" + newuuid) 
//      println("leafSet Nill") 
//      println(leafSet(idNill)) 
    } 
    else{ 
      val smallindex = this.leafSet.indices.reduceLeft{(x,y) => if(leafSet(x) < leafSet(y)) x else y} 
      val largeindex = this.leafSet.indices.reduceLeft{(x,y) => if(leafSet(x) > leafSet(y)) x else y} 
      if(newuuid>=leafSet(smallindex)&&newuuid<=leafSet(largeindex)){ 
        if(newuuid > this.uuid) leafSet(largeindex) = newuuid 
        else if(newuuid < this.uuid) leafSet(smallindex) = newuuid 
      } 
    } 
  } 
  
  def insertRoutingTable(newuuid: Int) = { 
    val shl = this.prefix(uuid , newuuid)
    val col = ((newuuid << (b*shl)) >>> (32-b)) 
//    if(shl==32/b) context.system.shutdown
    if (routingTable(shl*cols+col) == Xnull.value) routingTable(shl*cols+col) = newuuid
  } 

  def insertNode(newuuid: Int) = { 
    insertLeafSet(newuuid)
    insertRoutingTable(newuuid) 
  } 
  
  def printArray(s: Array[Int]) = { 
    for(index <- 0 to s.length-1) 
      print(s(index) + " ") 
    println()
  } 

  def route(rcvuuid: Int):(Int, Boolean) = {
//    println(uuid+" is routing "+rcvuuid) 
    val smalluuid = leafSet.reduceLeft( _ min _)
    val largeuuid = leafSet.reduceLeft( _ max _)
    if(leafSet.isEmpty||smalluuid==Xnull.value){ 
//      println("leafSet is Empty") 
      return (Xnull.value, true) 
    } 
    //case 1
    if(rcvuuid >= smalluuid && rcvuuid <= largeuuid){ 
//      println("case 1") 
      return (Xnull.value, true) 
    } 
    //case 2 
    else{ 
//      println("case 2") 
      val shl = this.prefix(uuid , rcvuuid) 
      val col = ((rcvuuid << (b*shl)) >>> (32-b)) 
      val fuuid = routingTable(shl*cols+col)
      if(fuuid!=Xnull.value)
      return (fuuid,false)

      //case 3 assume as case 1 since this is rare case
      else{ 
//      var ls: Array[Int] 
//      val shl = this.prefix(rcvuuid) 
//      Array.copy(routingTable, shl*cols, ls, 0, cols) 
//      println("case3") 
      var ls = leafSet ++ routingTable 
      for(index <- 0 to ls.size-1){
        if(ls(index)!=Xnull.value && (prefix(ls(index),rcvuuid) >= prefix(uuid,rcvuuid)) && Math.abs(ls(index)-rcvuuid) < Math.abs(this.uuid-rcvuuid)){ 
                return (ls(index), false) 
            } 
     } 
//      println("default case") 
      return (Xnull.value, true) 
      }
    }  
  }

  def receive = { 
    case ("uuid", rcvuuid: Int) => { 
      this.uuid = rcvuuid 
    } 
    case("Join", false, startuuid: Int, lastuuid: Int) => {
//      println("current ID:"+this.uuid) 
//      println("startuuid:"+startuuid) 
      var startnode = context.actorSelection("../"+startuuid.toString) 
      val lastnode = context.actorSelection("../"+lastuuid.toString)
      val (nextuuid,arrive) = route(startuuid) 
      val shl = this.prefix(uuid,startuuid) 
      var passrow = routingTable.filter(x => ( (x < (shl+1)*cols) && (x > shl*cols)))
      
      
      if(arrive && nextuuid == Xnull.value){ 
//        println("\n" + uuid + " leafSet:") 
//        printArray(this.leafSet)
//        println(uuid + " :pass leafset to " + startuuid +" \n")
        startnode ! ("leafSet", leafSet.clone)
        startnode ! ("ZID", this.uuid) 
      } 
      else if(!arrive&&nextuuid!=Xnull.value){ 
        val nextnode = context.actorSelection("../"+nextuuid.toString) 
        nextnode ! ("Join", false, startuuid, nextuuid) 
      } 
    }

    case("leafSet", fwleafSet: Array[Int]) => { 
//      println("fwleafSet")
//      printArray(fwleafSet) 
      fwleafSet.copyToArray(this.leafSet) 
//      println("\n" + uuid + ":this.leafSet\n")
//      printArray(leafSet) 
//      println("end LeafSet") 
    } 
    
    case("leafID", rcvuuid: Int) => {
//      println()
//      println("In LeafID") 
//      println("leafID rcvuuid:"+rcvuuid)
//      println("leafID this.uuid:"+this.uuid)
      insertNode(rcvuuid) 
//      println("end LeafID")
//      println() 
    } 

    case("ZID", rcvuuid:Int) => { 
//        println("In ZID")
//        println("ZID rcvuuid:"+rcvuuid)
//        println("ZID this.uuid:"+this.uuid) 
        insertNode(rcvuuid)
//        println("In ZID afterinsertNode") 
        for(index <- 0 to leafSet.length-1){ 
          if(leafSet(index)!=Xnull.value){ 
            var leafnode = context.actorSelection("../"+leafSet(index)) 
            leafnode ! ("leafID", uuid) 
          } 
        }
//        println(uuid + " leafset")
//        for(index <- 0 to leafSet.length-1) 
//        println(leafSet(index)) 

        context.parent ! ("Joined") 
      } 


    case("routingTable", rcvshl: Int, rcvarray: Array[Int]) => { 
      Array.copy(rcvarray, 0, this.routingTable, rcvshl*cols, cols) // array should transfer the null 
    } 

    case("Msg", false, hops: Int, desuuid: Int) => { 
//      println("hops"+hops)
      val(nextuuid,arrive) = route(desuuid) 
      if(arrive && nextuuid == Xnull.value){ 
        context.parent ! ("Msg Arrived", hops) 
      } 
      else if(!arrive&&nextuuid!=Xnull.value){
        val nextnode = context.actorSelection("../"+nextuuid.toString) 
        nextnode ! ("Msg",false, hops+1, desuuid) 
      } 
    } 
    
    case("Fail") => { 
      context.stop(self)  
    } 

    case("PUUID") => { 
//        println("PUUID:"+this.uuid) 
      }

    case("PLEAFSET") => { 
//      println(leafSet.mkString) 
    } 
  } 
} 

  class PastryBoss extends PastryNode{
    var nodenum, count, sum, rqstnum = 0 
    var uuidlist: List[Int] = List()   
    override def receive = { 
      case("Init", rcvnodenum:Int, rcvrqstnum: Int) => { 
          nodenum = rcvnodenum 
          rqstnum = rcvrqstnum 
          val rand = new Random 
          var matrixnodeuuid = rand.nextInt(Idrange.v)
          var matrixnode = context.actorOf(Props[PastryNode], matrixnodeuuid.toString)
          matrixnode ! ("uuid", matrixnodeuuid)
          uuidlist :+= matrixnodeuuid 
//          println("matrixuuid:"+matrixnodeuuid) 
          var nodeuuid = rand.nextInt(Idrange.v) 
            while(uuidlist.exists{_ == nodeuuid}) 
              nodeuuid = rand.nextInt(Idrange.v)
            var node = context.actorOf(Props[PastryNode], nodeuuid.toString)
            node ! ("uuid", nodeuuid) 
            uuidlist :+= nodeuuid 
//            println("nodeuuid"+nodeuuid) 


            matrixnode ! ("PUUID") 
            node ! ("PUUID") 



            matrixnode ! (("Join", false, nodeuuid, matrixnodeuuid)) //a copy of node OR ref of  node or uuid?  
        } 
        

        case("Joined") => { 
          count+=1 
//          println("count:"+count) 
          if(count < nodenum){ 
            val rand = new Random
            var nodeuuid = rand.nextInt(Idrange.v) 
            while(uuidlist.exists{_ ==nodeuuid}) 
              nodeuuid = rand.nextInt(Idrange.v) 
            var node = context.actorOf(Props[PastryNode], nodeuuid.toString) 
            node ! ("uuid", nodeuuid) 
            var matrixnodeuuid = uuidlist(rand.nextInt(uuidlist.length))
            while(matrixnodeuuid==0)
              matrixnodeuuid = uuidlist(rand.nextInt(uuidlist.length))
            var matrixnode = context.actorSelection(matrixnodeuuid.toString) 
            uuidlist :+=nodeuuid
//            println("\n===============================================\n")
            matrixnode ! (("Join", false, nodeuuid, matrixnodeuuid)) //a copy of node OR ref of  node or uuid?
          } 


 
          if(count>=nodenum){
            count = 0
            for(desindex <-0 to uuidlist.length-1){
              var desuuid = uuidlist(desindex)
              var desnode = context.actorSelection(desuuid.toString) 
              desnode ! ("PLEAFSET")
            } 






            for(desindex <- 0 to uuidlist.length-1){ 
              var desuuid = uuidlist(desindex) 
              val rand = new Random 
              for(rqstindex <- 0 to rqstnum-1){ 
                var strtindex = rand.nextInt(uuidlist.length) //-1?
                while(strtindex == desindex) strtindex = rand.nextInt(uuidlist.length) 
                var strtuuid = uuidlist(strtindex) 
                var strtnode = context.actorSelection( strtuuid.toString) 
                var desnode = context.actorSelection( desuuid.toString) 
                strtnode ! ("Msg", false, 0, desuuid)
              } 
            } 
          }  
        }

        case("Msg Arrived", hops: Int) => { 
          count += 1 
          sum += hops
//          println("sum:"+sum) 
          if(count == nodenum/100){ 
            val rand = new Random 
            var failindex = rand.nextInt(uuidlist.length) 
            var failuuid = uuidlist(failindex) 
            var failnode = context.actorSelection(failuuid.toString) 
            failnode ! "Fail" 
          } 

          if(count>=nodenum*rqstnum){ 
            println(sum.toDouble/(nodenum.toDouble*rqstnum.toDouble))
            context.system.shutdown
          } 
        } 
      } 
    } 

object project3 { 
  def main(args: Array[String]): Unit = { 
    val system = ActorSystem("project3") 
    val pastryboss = system.actorOf(Props(new PastryBoss), "pastryboss") 
     pastryboss ! ("Init", args(0).toInt, args(1).toInt) 
  } 
} 







   









