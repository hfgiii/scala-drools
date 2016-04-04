package org.hfgiii.drools.knowledgepackage

import scala.collection.JavaConversions._
import collection.mutable.LinkedHashMap._
import collection.mutable.{MutableList, LinkedHashMap}
import org.drools.definition.KnowledgePackage
import org.drools.builder.DecisionTableInputType
import org.hfgiii.drools.rulepackage.Ruleset
import org.hfgiii.drools.ruleflow.Ruleflow

trait DroolsPackageElement

case class DroolsDRLPath(path:String) extends  DroolsPackageElement
case class DroolsBPMNPath(path:String) extends  DroolsPackageElement
case class DroolsDecisionTablePath(path:String,dtType:DecisionTableInputType) extends  DroolsPackageElement

class DroolsKnowledgePackage(name:String=null) {

    knowledgePackage =>

    def actualName = Option(DroolsKnowledgePackage.this.name) getOrElse DroolsKnowledgePackage.this.getClass.getPackage.getName

    val rulesets:LinkedHashMap[String,Ruleset]       = empty[String,Ruleset]
    val ruleflows:LinkedHashMap[String,Ruleflow]     = empty[String,Ruleflow]
    val packageGlobals: LinkedHashMap[String,AnyRef] = empty[String,AnyRef]
    val drlPaths: MutableList[DroolsDRLPath]   = MutableList[DroolsDRLPath]()
    val bpmnPaths: MutableList[DroolsBPMNPath] = MutableList[DroolsBPMNPath]()
    val decisionTablePaths :  MutableList[DroolsDecisionTablePath] = MutableList[DroolsDecisionTablePath]()

    def Global[T <: AnyRef : Manifest](global: (String, T)) {
      packageGlobals += global
    }

    def addRuleflow(rf:Ruleflow) {
      ruleflows += (rf.name -> rf)
    }

    def addRuleflow(path:String) {
      bpmnPaths +=  DroolsBPMNPath(path)
    }

    def addRuleset(rs:Ruleset) {
      rulesets += (rs.name -> rs)
    }

    def addBPMNPath(path:String) {
      bpmnPaths +=  DroolsBPMNPath(path)
    }

    def addDRLPath(path:String) {
      drlPaths  +=  DroolsDRLPath(path)
    }

    def addDecisionTablePath(path:String,dtType:DecisionTableInputType) {
      decisionTablePaths  +=  DroolsDecisionTablePath(path,dtType)
    }


   def ruleflow(rf:Ruleflow):Ruleflow = {

     knowledgePackage.addRuleflow(rf)

     rf
   }


   def ruleflows(rfs:Ruleflow*) {
     rfs.foreach(knowledgePackage.addRuleflow(_))
   }


   def ruleset(rs:Ruleset):Ruleset = {

     knowledgePackage.addRuleset(rs)

     rs
   }

  def drlPaths(drls:String*) {
    drls.foreach(knowledgePackage.addDRLPath(_))
  }

  def bpmnPaths(bpmns:String *) {
    bpmns.foreach(knowledgePackage.addBPMNPath(_))
  }

  def decisionTablePaths(dtPaths:Tuple2[String,DecisionTableInputType] *) {
    dtPaths.foreach(dt => knowledgePackage.addDecisionTablePath(dt._1,dt._2))
  }

  def rulesets(rss:Ruleset*) {
     rss.foreach(knowledgePackage.addRuleset(_))
  }

  def build(implicit buldr:DroolsKnowledgeBuilder = new DroolsKnowledgeBuilder) {
     builder = Option(buldr)
     val kpBuilder = new KnowledgePackageBuilder(this)
     kpBuilder.build()
   }

   private var builder: Option[DroolsKnowledgeBuilder] = None

   def builtKnowledgePackage: KnowledgePackage = {
         builder.getOrElse(throw new IllegalStateException("Could not obtain package: name='%s'".format(actualName))).
         knowledgePackages find (_.getName == actualName) getOrElse {
            throw new IllegalStateException("Could not obtain package: name='%s'".format(actualName))
         }
      }
}

object DroolsKnowledgePackage {

   private var knowledgePackage:Option[DroolsKnowledgePackage] = None

   private implicit def optionToKnowledgePackage(opt:Option[DroolsKnowledgePackage]):DroolsKnowledgePackage   = opt.get
   private implicit def knowledgePackageToOption(kpack:DroolsKnowledgePackage):Option[DroolsKnowledgePackage] = Option(kpack)

   def newRuleflow(name:String):Ruleflow = {
     val rf = Ruleflow(name)
     knowledgePackage.addRuleflow(rf)

     rf
   }

   def ruleflows(rfs:Ruleflow*) {
     rfs.foreach(knowledgePackage.addRuleflow(_))
   }


   def newRuleset(name:String):Ruleset = {
     val rs =  new Ruleset(name)

     knowledgePackage match {
       case Some(kpkg) => kpkg.addRuleset(rs)
       case None       =>
     }

     rs
   }

   def rulesets(rss:Ruleset*) {
     rss.foreach(knowledgePackage.addRuleset(_))
   }

   def drlPaths(drls:String*) {
     drls.foreach(knowledgePackage.addDRLPath(_))
   }

   def bpmnPaths(bpmns:String *) {
     bpmns.foreach(knowledgePackage.addBPMNPath(_))
   }

   def knowledgePackage(name:String = null)(seqs: => Unit):DroolsKnowledgePackage = {
       knowledgePackage = new DroolsKnowledgePackage(name)

       seqs

       knowledgePackage
   }

    def knowledgePackages(rulesets:Ruleset*)(seqs: => Unit):DroolsKnowledgePackage = {
       knowledgePackage = new DroolsKnowledgePackage("")

       seqs

       val kpgs = new MutableList[DroolsKnowledgePackage]

       rulesets.foreach(ruleset => {
         val kpg = new DroolsKnowledgePackage(ruleset.name)
         kpg.ruleset(ruleset)
         kpgs += kpg
       })

       kpgs += knowledgePackage


       new DroolsKnowledgePackage() {
         override def build(implicit buldr:DroolsKnowledgeBuilder = new DroolsKnowledgeBuilder) {
              kpgs.foreach(_.build)
         }
       }
   }

  implicit def toKnowledgePackage(ruleset:Ruleset):DroolsKnowledgePackage =
      knowledgePackage() {
       rulesets (ruleset)
  }

}

