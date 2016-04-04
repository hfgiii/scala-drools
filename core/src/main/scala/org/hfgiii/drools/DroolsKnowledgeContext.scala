package org.hfgiii.drools

import org.drools.builder.DecisionTableInputType
import org.hfgiii.drools.knowledgepackage._
import org.hfgiii.drools.ruleflow.Ruleflow
import org.hfgiii.drools.rulepackage.Ruleset

import scala.collection.mutable.MutableList

trait DroolsKnowledgeContext {
  private var knowledgePackage = new DroolsKnowledgePackage

  def addRuleset(rs:Ruleset) {
      knowledgePackage.addRuleset(rs)
  }

  def addRulesets(rss:Ruleset*) {
     knowledgePackage.rulesets(rss :_*)
  }

  def addDecisionTablePaths(dtPaths:(String,DecisionTableInputType) *) {
     knowledgePackage.decisionTablePaths(dtPaths :_*)
  }

  def addRulesetsAsPackages(rss:Ruleset*) {
    val kpgs = new MutableList[DroolsKnowledgePackage]

        rss.foreach(ruleset => {
          val kpg = new DroolsKnowledgePackage(ruleset.name)
          kpg.ruleset(ruleset)
          kpgs += kpg
        })

        kpgs += knowledgePackage

        knowledgePackage =
          new DroolsKnowledgePackage() {
             override def build(implicit buldr:DroolsKnowledgeBuilder = new DroolsKnowledgeBuilder) {
                kpgs.foreach(_.build)
             }
          }
  }


  def addRuleflow(path:String) {
      knowledgePackage.addRuleflow(path)
  }

  def addRuleflow(ruleflow:Ruleflow) {
      knowledgePackage.addRuleflow(ruleflow)
  }

   def buildKnowledge(implicit buldr:DroolsKnowledgeBuilder)  {
     knowledgePackage.build
   }
}