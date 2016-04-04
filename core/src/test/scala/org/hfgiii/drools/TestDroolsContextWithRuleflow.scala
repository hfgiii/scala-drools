package org.hfgiii.drools

import org.drools.spi.KnowledgeHelper
import org.hfgiii.drools.condition.DroolsConditionGenerator._
import org.hfgiii.drools.rulepackage.Ruleset
import org.specs2.mutable.Specification


case class FactUn(name: String)

case class FactDeux(name: String, f: FactUn)

case class FactTrois(name: String, f: FactUn)

case class FactQuatre(name: String)

case class FactCinq(name: String)

case class FactSept(name: String)

case class FactUnSub(subname: String) extends FactUn(subname)

class TestDroolsContextWithRuleflow extends Specification with DroolsContext {


  lazy val f1_1 = FactUn("f1_1#instance")
  lazy val f1_2 = FactUn("f1_2#instance")
  lazy val f2_1 = FactDeux("f2_1#instance", f1_1)
  lazy val f2_2 = FactDeux("f2_1#instance", f1_2)
  lazy val f3_1 = FactTrois("f3_1#instance", f1_1)
  lazy val f3_2 = FactTrois("f3_1#instance", f1_2)
  lazy val f4_1 = FactQuatre("f4_1#instance")
  lazy val f4_2 = FactQuatre("f4_2#instance")
  lazy val f5_1 = FactCinq("f5_1#instance")
  lazy val f5_2 = FactCinq("f5_2#instance")
  lazy val f6_1 = FactSept("f5_1#instance")
  lazy val f6_2 = FactSept("f5_2#instance")
  lazy val f1s_1 = FactUnSub("f1s_1#instance")

  var actual_f1_1: FactUn = _
  var actual_f1_2: FactUn = _
  var actual_f2_1: FactDeux = _
  var actual_f2_2: FactDeux = _
  var actual_f3_1: FactTrois = _
  var actual_f3_2: FactTrois = _
  var actual_f4_1: FactQuatre = _
  var actual_f4_2: FactQuatre = _
  var actual_f5_1: FactCinq = _
  var actual_f5_2: FactCinq = _
  var actual_f6_1: FactSept = _
  var actual_f6_2: FactSept = _

   def insertAllFactsAndRunProcess(process:String) {
    buildKnowledge
    newKnowledgeSession
    insertFacts(f1_1
              ,f1_2
              ,f2_1
              ,f2_2
              ,f3_1
              ,f3_2
              ,f4_1
              ,f4_2
              ,f5_1
              ,f5_2
              ,f6_1
              ,f6_2)

    runProcess(process)
  }

  sequential

  "Ruleflows from files" should {
    "Tester ruleflow avec quatre groupes et ConDivergingANDGateways de fichier" in {

      val ha = new Ruleset("ha") {

        Import[FactUn]
        Import[FactDeux]

        Rule("rule1",
          ruleflowGroup = "hagruppe").When {
          'f1_10 := ('FactUn -> nothing)
          'f2_10 := ('FactDeux -> nothing)
        } Then { (f1_10: FactUn, f2_10: FactDeux) =>
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10
        }
      }

      val haha = new Ruleset("haha") {
        Import[FactTrois]
        Import[FactQuatre]

        Rule("rule2",
          ruleflowGroup = "hahagruppe").When {
          'f3_10 := ('FactTrois -> nothing)
          'f4_10 := ('FactQuatre -> nothing)
        } Then { (f3_10: FactTrois, f4_10: FactQuatre) =>
          actual_f3_1 = f3_10
          actual_f4_1 = f4_10
        }
      }

      val hoho = new Ruleset("hoho") {
        Import[FactSept]
        Import[FactCinq]

        Rule("rule3",
          ruleflowGroup = "hohogruppe").When {
          'f5_10 := ('FactCinq -> nothing)
          'f6_10 := ('FactSept -> nothing)
        } Then { (f5_10: FactCinq, f6_10: FactSept) =>
          actual_f5_1 = f5_10
          actual_f6_1 = f6_10
        }
      }

      val hehe = new Ruleset("hehe") {
        Import[FactUn]
        Import[FactSept]

        Rule("rule4",
          ruleflowGroup = "hehegruppe").When {
          'f1_11 := ('FactUn -> nothing)
          'f6_11 := ('FactSept -> nothing)
        } Then { (f1_11: FactUn, f6_11: FactSept) =>
          actual_f1_2 = f1_11
          actual_f6_2 = f6_11
        }
      }

      addRulesets(ha, haha, hoho, hehe)

      addRuleflow("fourgroups.bpmn")

      insertAllFactsAndRunProcess("fourgroups")

      actual_f1_1 === f1_1 and
        actual_f2_1 === f2_1 and
        actual_f5_1 === f5_1 and
        actual_f6_1 === f6_1 and
        actual_f1_2 === f1_1 and
        actual_f6_2 === f6_1 and
        actual_f3_1 === f3_1 and
        actual_f4_1 === f4_1
    }
  }

  "Zot2" should {
    "Tester ruleflow avec trois groupes et ExclusiveXORGatewayLeftLeg de fichier" in {

      val ha = new Ruleset("ha") {

        Import[FactUn]
        Import[FactDeux]

        Rule("rule1",
          ruleflowGroup = "hagruppe",
          lockOnActive = true).When {
          'f1_10 := ('FactUn -> nothing)
          'f2_10 := ('FactDeux -> nothing)
        } Then { (f1_10: FactUn, f2_10: FactDeux, kh: KnowledgeHelper) =>
          f1_10.copy(name = "blahblah")
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10

          kh update f1_10
        }
      }

      val haha = new Ruleset("haha") {
        Import[FactTrois]
        Import[FactQuatre]

        Rule("rule2",
          ruleflowGroup = "hahagruppe").When {
          'f3_10 := ('FactTrois -> nothing)
          'f4_10 := ('FactQuatre -> nothing)
        } Then { (f3_10: FactTrois, f4_10: FactQuatre) =>
          actual_f3_1 = f3_10
          actual_f4_1 = f4_10
        }
      }

      val hoho = new Ruleset("hoho") {
        Import[FactSept]
        Import[FactCinq]

        Rule("rule3",
          ruleflowGroup = "hohogruppe").When {
          'f5_10 := ('FactCinq -> nothing)
          'f6_10 := ('FactSept -> nothing)
        } Then { (f5_10: FactCinq, f6_10: FactSept) =>
          actual_f5_1 = f5_10
          actual_f6_1 = f6_10
        }
      }

      addRulesets(ha, haha, hoho)

      addRuleflow("troisgroupes.bpmn")

      insertAllFactsAndRunProcess("troisgroupes")

      actual_f1_1 === f1_1 and
        actual_f2_1 === f2_1 and
        actual_f5_1 === f5_1 and
        actual_f6_1 === f6_1

      actual_f3_1 !== f3_1
      actual_f4_1 !== f4_1

    }
  }
}