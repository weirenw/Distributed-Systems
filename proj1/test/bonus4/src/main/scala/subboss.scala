 package bonus
  
import akka.actor._;
import akka.actor.{Actor, PoisonPill};
import scala.math._;
import com.typesafe.config.ConfigFactory;
import akka.actor.{ ActorRef, Props, Actor, ActorSystem };
import akka.kernel.Bootable;
import akka.actor.ReceiveTimeout;

class SubBossListen extends Bootable {

     val system = ActorSystem("SubBossListen", ConfigFactory.load.getConfig("subbosslisten"))
     
     def startup(){
     }

     def  shutdown(){
       system.shutdown()
     }
   }
object SubApp{
  def main(args: Array[String]){
    new SubBossListen
  } 
} 

