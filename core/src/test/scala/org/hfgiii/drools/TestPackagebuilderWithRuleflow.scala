package org.hfgiii.drools

import org.drools.spi.KnowledgeHelper
import org.hfgiii.drools.condition.DroolsConditionGenerator._
import org.hfgiii.drools.condition.DroolsNullCondition
import org.hfgiii.drools.knowledgepackage.DroolsKnowledgePackage._
import org.hfgiii.drools.ruleflow.Ruleflow
import org.hfgiii.drools.rulepackage.Ruleset
import org.specs2.mutable.Specification


case class FactEins(name: String)

case class FactZwei(name: String, f: FactEins)

case class FactDrei(name: String, f: FactEins)

case class FactVier(name: String)

case class FactFuenf(name: String)

case class FactSechs(name: String)

case class FactEinsSub(subname: String) extends FactEins(subname)

class TestPackagebuilderWithRuleflow extends Specification with DroolsRuntimeContext {

  lazy val f1_1 = new FactEins("f1_1#instance")
  lazy val f1_2 = new FactEins("f1_2#instance")
  lazy val f2_1 = new FactZwei("f2_1#instance", f1_1)
  lazy val f2_2 = new FactZwei("f2_1#instance", f1_2)
  lazy val f3_1 = new FactDrei("f3_1#instance", f1_1)
  lazy val f3_2 = new FactDrei("f3_1#instance", f1_2)
  lazy val f4_1 = new FactVier("f4_1#instance")
  lazy val f4_2 = new FactVier("f4_2#instance")
  lazy val f5_1 = new FactFuenf("f5_1#instance")
  lazy val f5_2 = new FactFuenf("f5_2#instance")
  lazy val f6_1 = new FactSechs("f5_1#instance")
  lazy val f6_2 = new FactSechs("f5_2#instance")
  lazy val f1s_1 = new FactEinsSub("f1s_1#instance")

  var actual_f1_1: FactEins = _
  var actual_f1_2: FactEins = _
  var actual_f2_1: FactZwei = _
  var actual_f2_2: FactZwei = _
  var actual_f3_1: FactDrei = _
  var actual_f3_2: FactDrei = _
  var actual_f4_1: FactVier = _
  var actual_f4_2: FactVier = _
  var actual_f5_1: FactFuenf = _
  var actual_f5_2: FactFuenf = _
  var actual_f6_1: FactSechs = _
  var actual_f6_2: FactSechs = _

  implicit def ruleflowElementToTuple(dre:Ruleflow#DroolsRuleflowElement):(Ruleflow#DroolsRuleflowElement,DroolsNullCondition) =
   (dre,nothing)

  def insertAllFactsStartProcessAndFire(process:String) {

    newKnowledgeSession

    session insert f1_1
    session insert f1_2
    session insert f2_1
    session insert f2_2
    session insert f3_1
    session insert f3_2
    session insert f4_1
    session insert f4_2
    session insert f5_1
    session insert f5_2
    session insert f6_1
    session insert f6_2

    runProcess(process)

//    session startProcess process
//
//    session fireAllRules
  }


  def testSimplestRuleflow {
     knowledgePackage("anfangamende") {
        ruleflows {
         new Ruleflow("haRF"){
           start("haRF") >=> end("haRF")
         }
       }
     }.build
  }

  sequential

  "Ruleflows described by DSL and files" should {
    "Test ruleflow with one group" in {
      val ha = new Ruleset("ha") {
        Import[FactEins]
        Import[FactZwei]

        Rule("rule").When {
          'f1_10 := ('FactEins -> nothing)
          'f2_10 := ('FactZwei -> nothing)
        } Then { (f1_10: FactEins, f2_10: FactZwei) =>
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10
        }
      }

      knowledgePackage("unendlich") {

        rulesets {
          ha
        }

        ruleflows {
          new Ruleflow("haRF") {
            val hagruppe = ruleflowgroup("hagruppe") {
              ha
            }

            start("haRF") >=> hagruppe
            hagruppe >=> end("haRF")
          }
        }
      }.build


      insertAllFactsStartProcessAndFire("haRF")

      actual_f1_1 === f1_1
      actual_f2_1 === f2_1
    }

    "Test ruleflow with two groups" in {
      val ha = new Ruleset("ha") {

        Import[FactEins]
        Import[FactZwei]

        Rule("rule1").When {
          'f1_10 := ('FactEins -> nothing)
          'f2_10 := ('FactZwei -> nothing)
        } Then { (f1_10: FactEins, f2_10: FactZwei) =>
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10
        }
      }

      val haha = new Ruleset("haha") {
        Import[FactDrei]
        Import[FactVier]

        Rule("rule2").When {
          'f3_10 := ('FactDrei -> nothing)
          'f4_10 := ('FactVier -> nothing)
        } Then { (f3_10: FactDrei, f4_10: FactVier) =>
          actual_f3_1 = f3_10
          actual_f4_1 = f4_10
        }
      }

      knowledgePackage("unendlich") {

        rulesets(
          ha,
          haha
        )

        ruleflows {
          new Ruleflow("lachenRF") {
            val hagruppe = ruleflowgroup("hagruppe") {
              ha
            }
            val hahagruppe = ruleflowgroup("hahagruppe") {
              haha
            }

            start("lachenRF") >=> hagruppe
            hagruppe >=> hahagruppe
            hahagruppe >=> end("lachenRF")
          }
        }
      }.build


      insertAllFactsStartProcessAndFire("lachenRF")

      actual_f1_1 === f1_1
      actual_f2_1 === f2_1

      actual_f3_1 === f3_1
      actual_f4_1 === f4_1
    }

    "test ruleflow with four groups and ConDivergingANDGateways" in {
      import ruleflow.DroolsRuleflowGatewayLogicType._

      val ha = new Ruleset("ha") {

        Import[FactEins]
        Import[FactZwei]

        Rule("rule1").When {
          'f1_10 := ('FactEins -> nothing)
          'f2_10 := ('FactZwei -> nothing)
        } Then { (f1_10: FactEins, f2_10: FactZwei) =>
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10
        }
      }

      val haha = new Ruleset("haha") {
        Import[FactDrei]
        Import[FactVier]

        Rule("rule2").When {
          'f3_10 := ('FactDrei -> nothing)
          'f4_10 := ('FactVier -> nothing)
        } Then { (f3_10: FactDrei, f4_10: FactVier) =>
          actual_f3_1 = f3_10
          actual_f4_1 = f4_10
        }
      }

      val hoho = new Ruleset("hoho") {
        Import[FactSechs]
        Import[FactFuenf]

        Rule("rule3").When {
          'f5_10 := ('FactFuenf -> nothing)
          'f6_10 := ('FactSechs -> nothing)
        } Then { (f5_10: FactFuenf, f6_10: FactSechs) =>
          actual_f5_1 = f5_10
          actual_f6_1 = f6_10
        }
      }



      val hehe = new Ruleset("hehe") {
        Import[FactEins]
        Import[FactSechs]

        Rule("rule4").When {
          'f1_11 := ('FactEins -> nothing)
          'f6_11 := ('FactSechs -> nothing)
        } Then { (f1_11: FactEins, f6_11: FactSechs) =>
          actual_f1_2 = f1_11
          actual_f6_2 = f6_11
        }
      }

      knowledgePackage("unendlich") {

        rulesets(
          ha,
          haha,
          hoho,
          hehe
        )

        ruleflows {
          new Ruleflow("lachenRF") {

            //Ruleflow Nodes
            val hagruppe = ruleflowgroup("hagruppe") {
              ha
            }
            val hahagruppe = ruleflowgroup("hahagruppe") {
              haha
            }
            val hohogruppe = ruleflowgroup("hohogruppe") {
              hoho
            }
            val hehegruppe = ruleflowgroup("hehegruppe") {
              hehe
            }

            val andd = emptyDivergingGateway("andd", AND)
            var andc = emptyConvergingGateway("andc", AND)

            //Ruleflow Sequences
            start("lachenRF") >=> hagruppe
            hagruppe >=> andd
            andd -&<((hohogruppe, nothing), (hahagruppe, nothing))
            andc >&-(hohogruppe, hahagruppe)
            andc >=> hehegruppe
            hehegruppe >=> end("lachenRF")

          }
        }
      }.build


      insertAllFactsStartProcessAndFire("lachenRF")

      actual_f1_1 === f1_1
      actual_f2_1 === f2_1

      actual_f5_1 === f5_1
      actual_f6_1 === f6_1

      actual_f1_2 === f1_1
      actual_f6_2 === f6_1

      actual_f3_1 === f3_1
      actual_f4_1 === f4_1


    }


    "Test ruleflow with four groups and ConDivergingANDGateways from file" in {

      val ha = new Ruleset("ha") {

        Import[FactEins]
        Import[FactZwei]

        Rule("rule1", ruleflowGroup = "hagruppe").When {
          'f1_10 := ('FactEins -> nothing)
          'f2_10 := ('FactZwei -> nothing)
        } Then { (f1_10: FactEins, f2_10: FactZwei) =>
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10
        }
      }

      val haha = new Ruleset("haha") {

        Import[FactDrei]
        Import[FactVier]

        Rule("rule2",ruleflowGroup = "hahagruppe").When {
          'f3_10 := ('FactDrei -> nothing)
          'f4_10 := ('FactVier -> nothing)
        } Then { (f3_10: FactDrei, f4_10: FactVier) =>
          actual_f3_1 = f3_10
          actual_f4_1 = f4_10
        }
      }

      val hoho = new Ruleset("hoho") {

        Import[FactSechs]
        Import[FactFuenf]

        Rule("rule3", ruleflowGroup = "hohogruppe").When {
          'f5_10 := ('FactFuenf -> nothing)
          'f6_10 := ('FactSechs -> nothing)
        } Then { (f5_10: FactFuenf, f6_10: FactSechs) =>
          actual_f5_1 = f5_10
          actual_f6_1 = f6_10
        }
      }



      val hehe = new Ruleset("hehe") {

        Import[FactEins]
        Import[FactSechs]

        Rule("rule4", ruleflowGroup = "hehegruppe").When {
          'f1_11 := ('FactEins -> nothing)
          'f6_11 := ('FactSechs -> nothing)
        } Then { (f1_11: FactEins, f6_11: FactSechs) =>
          actual_f1_2 = f1_11
          actual_f6_2 = f6_11
        }
      }

      knowledgePackage("unendlich") {

        rulesets(
          ha,
          haha,
          hoho,
          hehe
        )
        bpmnPaths("fourgroups.bpmn")
      }.build


      insertAllFactsStartProcessAndFire("fourgroups")

      actual_f1_1 === f1_1
      actual_f2_1 === f2_1

      actual_f5_1 === f5_1
      actual_f6_1 === f6_1

      actual_f1_2 === f1_1
      actual_f6_2 === f6_1

      actual_f3_1 === f3_1
      actual_f4_1 === f4_1


    }

    "Test ruleflow with three groups and ExclusiveXORGateway left leg" in {

      import ruleflow.DroolsRuleflowGatewayLogicType._

      val ha = new Ruleset("ha") {

        Import[FactEins]
        Import[FactZwei]

        Rule("rule1", lockOnActive = true).When {
          'f1_10 := ('FactEins -> nothing)
          'f2_10 := ('FactZwei -> nothing)
        } Then { (f1_10: FactEins, f2_10: FactZwei, kh: KnowledgeHelper) =>
          f1_10.copy(name = "blahblah")
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10

          kh update f1_10
        }
      }

      val haha = new Ruleset("haha") {
        Import[FactDrei]
        Import[FactVier]

        Rule("rule2").When {
          'f3_10 := ('FactDrei -> nothing)
          'f4_10 := ('FactVier -> nothing)
        } Then { (f3_10: FactDrei, f4_10: FactVier) =>
          actual_f3_1 = f3_10
          actual_f4_1 = f4_10
        }
      }

      val hoho = new Ruleset("hoho") {
        Import[FactSechs]
        Import[FactFuenf]

        Rule("rule3").When {
          'f5_10 := ('FactFuenf -> nothing)
          'f6_10 := ('FactSechs -> nothing)
        } Then { (f5_10: FactFuenf, f6_10: FactSechs) =>
          actual_f5_1 = f5_10
          actual_f6_1 = f6_10
        }
      }


      knowledgePackage("unendlich") {

        rulesets(
          ha,
          haha,
          hoho
        )

        ruleflows {
          new Ruleflow("lachenRF") {
            Import[FactEins]

            //Ruleflow Nodes
            val hagruppe = ruleflowgroup("hagruppe") {
              ha
            }
            val hahagruppe = ruleflowgroup("hahagruppe") {
              haha
            }
            val hohogruppe = ruleflowgroup("hohogruppe") {
              hoho
            }

            var xorc = emptyConvergingGateway("xorc", XOR)
            val xord = emptyDivergingGateway("xord", XOR)
            val xord1 = emptyDivergingGateway("xord1", XOR)

            //Ruleflow Sequences
            start("lachenRF") >=> hagruppe
            hagruppe >=> xord
            xord -<((hohogruppe, 'FactEins -> ^('name === "blahblah")), (xorc, 'FactEins -> ^('name !== "blahblah")))
            hohogruppe >=> xord1
            xord1 -<((xorc, 'FactEins -> ^('name === "blahblah")), (hahagruppe, 'FactEins -> ^('name !== "blahblah")))
            hahagruppe >=> xorc
            xorc >=> end("lachenRF")

          }
        }
      }.build


      insertAllFactsStartProcessAndFire("lachenRF")

      actual_f1_1 === f1_1
      actual_f2_1 === f2_1

      actual_f5_1 === f5_1
      actual_f6_1 === f6_1

      actual_f3_1 must_!= f3_1
      actual_f4_1 must_!= f4_1

    }

    "Test ruleflow with three groups and ExclusiveXORGateway left leg from file" in {

      val ha = new Ruleset("ha") {

        Import[FactEins]
        Import[FactZwei]

        Rule("rule1", ruleflowGroup = "hagruppe", lockOnActive = true).When {
          'f1_10 := ('FactEins -> nothing)
          'f2_10 := ('FactZwei -> nothing)
        } Then { (f1_10: FactEins, f2_10: FactZwei, kh: KnowledgeHelper) =>
          f1_10.copy(name = "blahblah")
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10

          kh update f1_10
        }
      }

      val haha = new Ruleset("haha") {

        Import[FactDrei]
        Import[FactVier]

        Rule("rule2", ruleflowGroup = "hahagruppe").When {
          'f3_10 := ('FactDrei -> nothing)
          'f4_10 := ('FactVier -> nothing)
        } Then { (f3_10: FactDrei, f4_10: FactVier) =>
          actual_f3_1 = f3_10
          actual_f4_1 = f4_10
        }
      }

      val hoho = new Ruleset("hoho") {

        Import[FactSechs]
        Import[FactFuenf]

        Rule("rule3", ruleflowGroup = "hohogruppe").When {
          'f5_10 := ('FactFuenf -> nothing)
          'f6_10 := ('FactSechs -> nothing)
        } Then { (f5_10: FactFuenf, f6_10: FactSechs) =>
          actual_f5_1 = f5_10
          actual_f6_1 = f6_10
        }
      }


      knowledgePackage("unendlich") {

        rulesets(
          ha,
          haha,
          hoho
        )

        bpmnPaths("threegroups.bpmn")
      }.build


      insertAllFactsStartProcessAndFire("threegroups")

      actual_f1_1 === f1_1
      actual_f2_1 === f2_1

      actual_f5_1 === f5_1
      actual_f6_1 === f6_1

      actual_f3_1 !== f3_1
      actual_f4_1 !== f4_1

    }
  }
}