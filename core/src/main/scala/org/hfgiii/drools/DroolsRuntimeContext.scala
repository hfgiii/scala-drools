package org.hfgiii.drools

import scala.collection.JavaConversions._
import org.drools.conf.EventProcessingOption
import org.drools.runtime.StatefulKnowledgeSession
import org.drools.runtime.conf.ClockTypeOption
import org.drools.common.DefaultFactHandle
import knowledgepackage.DroolsKnowledgeBuilder
import DroolsKnowledgeBuilder._

trait DroolsRuntimeContext extends DroolsLogging {

  val DEBUG : Boolean  = true

  implicit val builder = newBuilder

  lazy val knoBase     = newKnowledgeBase(builder.knowledgePackages, Seq(EventProcessingOption.STREAM))

  var session:StatefulKnowledgeSession = null

  def  newKnowledgeSession {
    session = knoBase.newScalaStatefulKnowledgeSession(Seq(ClockTypeOption.get("pseudo")))
  }

  def  disposeKnowledgeSession {

      session dispose

      session = null
  }

  def anySession:Boolean =
     Option(session) match {
       case Some(_) => true
       case None    => false
     }

  def insertFact ( businessObject : AnyRef) = {
    session insert businessObject
  }

  def insertFacts(bos:AnyRef*) {
     bos.foreach(session insert _)
  }

  def retractFact ( businessObject : AnyRef) = {
    session retractFact businessObject
  }

  def retractFact(bos:AnyRef*) {
     bos.foreach(session retractFact _)
  }

  def purgeWorkingMemory () =
        for {
          we <- session.getWorkingMemoryEntryPoints
          fh <- we.getFactHandles[DefaultFactHandle]
        } { session retract (fh)}

  def runRules (debugSession : Boolean=true,dispose:Boolean=true) : Unit = {
     if (debugSession == true) {
        debugAgenda(session)
        debugWorkingMemory(session)
     }
     session fireAllRules

     if(dispose) {
       disposeKnowledgeSession
     }

  }

  def runProcess(process:String,debugSession : Boolean=false,purge:Boolean=true) {
        //log("Process being tested is " + process)
        session startProcess(process)
        runRules(debugSession,purge)

  }

   override def log(message: String): Unit = println(message)
}