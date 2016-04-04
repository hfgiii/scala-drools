package org.hfgiii.drools

import org.drools.WorkingMemory
import org.drools.definition.KnowledgePackage
import org.drools.definitions.rule.impl.RuleImpl
import org.drools.spi.KnowledgeHelper
import org.hfgiii.drools.condition.DroolsConditionGenerator._
import org.hfgiii.drools.knowledgepackage.DroolsKnowledgePackage._
import org.hfgiii.drools.rulepackage.Ruleset
import org.specs2.mutable.Specification

import scala.collection.JavaConversions._


case class FactOne(val name: String)

case class FactTwo(val name: String, val f: FactOne)

case class FactThree(val name: String, val f: FactOne)

case class FactFour(val name: String)

case class FactOneSub(val subname: String) extends FactOne(subname)

class TestPackageBuilder extends Specification with DroolsRuntimeContext {

  val f1_1 = FactOne("f1_1#instance")
  val f1_2 = FactOne("f1_2#instance")
  val f2_1 = FactTwo("f2_1#instance", f1_1)
  val f2_2 = FactTwo("f2_1#instance", f1_2)
  val f3_1 = FactThree("f3_1#instance", f1_1)
  val f3_2 = FactThree("f3_1#instance", f1_2)
  val f4_1 = FactOne("f4_1#instance")
  val f4_2 = FactOne("f4_2#instance")
  val f1s_1 = FactOneSub("f1s_1#instance")

  var actual_f1_1: FactOne = _
  var actual_f1_2: FactOne = _
  var actual_f2_1: FactTwo = _
  var actual_f2_2: FactTwo = _
  var actual_f3_1: FactThree = _
  var actual_f3_2: FactThree = _
  var actual_f4_1: FactFour = _
  var actual_f4_2: FactFour = _


  def insertAllFactsAndFire {
    newKnowledgeSession
    session insert f1_1
    session insert f1_2
    session insert f2_1
    session insert f2_2
    session insert f3_1
    session insert f3_2
    session insert f4_1
    session insert f4_2
    runRules()
  }

  sequential

  "Zot" should {
    "consequence_invoked_with_args" in {

      new Ruleset("ha") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule").When {
          """
            f1_10: FactOne()
            f2_10: FactTwo()
          """
        } Then { (f1_10: FactOne, f2_10: FactTwo) =>
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10
        }
      }.build

      insertAllFactsAndFire
      actual_f1_1 === f1_1
      actual_f2_1 === f2_1
    }

    "consequence_invoked_with_args_and_condition_dsl" in {

      new Ruleset("ha") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule1") When {
          'f1_10 := ('FactOne -> nothing)
          'f2_10 := ('FactTwo -> nothing)
        } Then { (f1_10: FactOne, f2_10: FactTwo) =>
          actual_f1_1 = f1_10
          actual_f2_1 = f2_10
        }
      }.build

      insertAllFactsAndFire
      actual_f1_1 === f1_1
      actual_f2_1 === f2_1
    }

    "consequence_invoked_with_zero_args" in {
      var invoked = false

      new Ruleset("niente") {
        Import[FactOne]

        Rule("rule2").When {
          """
             f1_1: FactOne()
          """
        } Then { () =>
          invoked = true
        }
      }.build

      insertAllFactsAndFire
      invoked must beTrue
    }

    "consequence_invoked_with_args_when_args_are_out_of_order" in {
      new Ruleset("niente") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule").When {
          """
             f1_1: FactOne()
             f2_1: FactTwo()
          """
        } Then { (f2_1: FactTwo, f1_1: FactOne) =>
          actual_f1_1 = f1_1
          actual_f2_1 = f2_1
        }
      }.build

      insertAllFactsAndFire
      actual_f1_1 === f1_1
      actual_f2_1 === f2_1
    }

    "exception_thrown_for_missing_fact" in {
      new Ruleset("niente") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule").When {
          """
              f1_1: FactOne()
              f2_1: FactTwo()
          """
        } Then { (f1_1: FactOne, f2_1: FactTwo, f3_1: FactThree) =>
          // noop
        }
      }.build must throwA[builder.RuleBuildException]
    }

    "exception_thrown_for_arg_with_non_matching_names" in {
      new Ruleset("niente") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule").
          When {
            """
            f1_1: FactOne()
            f2_1: FactTwo()
            """
          } Then { (f1_xxx: FactOne, f2_1: FactTwo, f3_1: FactThree) =>
          // noop
        }
      }.build must throwA[builder.RuleBuildException]
      //    purgeWorkingMemory()
    }

    "exception_thrown_for_arg_with_non_matching_type" in {
      new Ruleset("niente") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule").When {
          """
               f1_1: FactOne()
               f2_1: FactTwo()
          """
        } Then { (f1_1: FactThree, f2_1: FactTwo)
        =>
          // noop
        }
      }.build must throwA[builder.RuleBuildException]


      //    purgeWorkingMemory()
    }



    "consequence_invoked_when_arg_supertype_of_fact" in {
      new Ruleset ("niente") {
        Import[FactOneSub]

        Rule("rule").When {
          """
             f1_1: FactOneSub()
          """
        } Then { f1_1:
                 FactOne =>
          actual_f1_1 = f1_1
        }
      }.build
      newKnowledgeSession
      session insert f1s_1
      session fireAllRules()
      actual_f1_1 === f1s_1
    }


    "exception_thrown_arg_subtype_of_fact" in {
      // really same as wrong type
      new Ruleset("niente") {
        Import[FactOne]

        Rule("rule").When {
          """
                   f1_1: FactOne()
          """
        } Then {

          f1_1: FactOneSub =>
          // noop
        }
      }.
        build must throwA[builder.RuleBuildException]

    }

    "consequence_invoked_with_special_args" in {
      var
      actual_kh: KnowledgeHelper = null
      var actual_wm: WorkingMemory =
        null

      new Ruleset("niente") {
        Import[FactOne]

        Rule(
          "rule").When {
          """
                     f1_1: FactOne()
          """
        } Then { (kh: KnowledgeHelper, wm: WorkingMemory) =>
          actual_kh = kh
          actual_wm = wm
        }
      }.build

      insertAllFactsAndFire

      actual_kh !== null
      actual_wm !== null
    }

    "consequence_invoked_with_args_and_special_args" in {
      var actual_kh:
      KnowledgeHelper = null
      var actual_wm: WorkingMemory = null

      new Ruleset("niente") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule").When {
          """
                   f1_1: FactOne()
                   f2_1: FactTwo()
          """
        } Then { (f1_1: FactOne,
                f2_1: FactTwo, kh: KnowledgeHelper, wm: WorkingMemory) =>
          actual_f1_1 = f1_1
          actual_f2_1 = f2_1

          actual_kh = kh
          actual_wm = wm
        }
      }.build

      insertAllFactsAndFire

      actual_f1_1 === f1_1
      actual_f2_1 === f2_1

      actual_kh !== null
      actual_wm !== null
    }


    "exception_thrown_for_malformed_lhs" in {
      new Ruleset("niente") {
        Import[FactOne]

        Rule("rule").When {
          """ f1_1: FactOne() asdf
          """
        } Then { f1_1: FactOne =>
          // noop
        }
      }.build must throwA[builder.RuleBuildException]

    }

    // Really same as mismatch parameter name (wrt implementation)
    "exception_thrown_for_missing_import" in {
      new Ruleset("niente") {
        Rule("rule").When {
          """
                f1_1: FactOne()
          """
        } Then { f1_1: FactOneSub =>
          // noop
        }
      }.build must throwA[builder.RuleBuildException]

      //   purgeWorkingMemory
    }

    "package_name_defaults_to_containing_scala_class_package" in {
      knowledgePackage() {
        rulesets {
          new Ruleset("niente") {

            Import[FactOne]
            Rule("rule").
              When {
                """
                   f1_1: FactOne()
                """
              } Then { (f1_1: FactOne) =>
              // noop
            }
          }
        }
      }.build

      builder.knowledgePackages.size must_==  1
      //builder.knowledgePackages.head.getName === classOf[DroolsKnowledgePackage].getPackage.getName
    }

    "package_named_explicitly" in {
      new Ruleset("niente") {
        Import[FactOne]
        Rule("rule").When {
          """
             f1_1: FactOne()
          """
        } Then { () =>
          // noop
        }
      }.build

      builder.knowledgePackages.size  must_== 1


      builder.knowledgePackages.head.getName  must_== "org.hfgiii.drools.knowledgepackage"

    }

    "multiple_packages_with_different_names" in {
      knowledgePackage("nada") {
        rulesets {
          new Ruleset("niente") {
            Import[FactOne]
            Rule("rule").When {
              """
                    f1_1: FactOne()
              """
            } Then { () =>
              // noop
            }
          }
        }
      }.build
      knowledgePackage("nadanada") {
        rulesets {
          new Ruleset(
            "nienteniente") {
            Import[FactOne]
            Rule("rule").When {
              """
                   f1_1: FactOne()
              """
            } Then { () =>
              // noop
            }
          }
        }
      }.build

      builder.knowledgePackages.size  must_== 2
      builder.knowledgePackages. map(_.getName).toSet === Set("nada", "nadanada")
//      builder. knowledgePackages.foreach { pkg =>
//        pkg.getRules.size  must_== 1
//        pkg. getRules.head.getName  must_== "rule"
//      }
      //  disposeKnowledgeSession
    }


    "multiple_packages_with_same_name_combine" in {
      knowledgePackage("nada") {
        rulesets {
          new Ruleset("niente") {
            Import[FactOne]
            Rule("rule_1").When {
              """
        f1_1: FactOne()
              """
            } Then { () =>
              // noop
            }
          }
        }
      }.build

      knowledgePackage("nada") {
        rulesets {
          new Ruleset("niente") {
            Import[FactOne]
            Rule("rule_2").When {
              """
                   f1_1: FactOne()
              """
            } Then { () =>
              // noop
            }
          }
        }
      }.build

      builder.knowledgePackages.size  must_== 1
      builder.knowledgePackages.head.getName  must_== "nada"
      builder.knowledgePackages.head.getRules.map(_.getName) === List("rule_1", "rule_2")
    }
    // This is the default drools behavior, but it would be nice to get a warning.

    "rule_with_same_name_overrides_previous" in {
      var rule1_invoked = false
      var rule2_invoked = false


      new Ruleset("niente") {
        Import[FactOne]
        Rule("rule").When {
          """
                      f1_1: FactOne()
          """
        } Then { () =>
          rule1_invoked = true
        }

        Rule("rule").When {
          """
                      f1_1: FactOne()
          """
        } Then { () =>
          rule2_invoked =
            true
        }
      }.build

      insertAllFactsAndFire
      rule1_invoked === false
      rule2_invoked === true
    }

    "rule_attributes_configured" in {

      new Ruleset("niente") {
        Import[FactOne]

        Rule("rule",salience = 10, agendaGroup = "agenda_group", ruleflowGroup = "ruleflow_group", lockOnActive = true, noLoop = true)
          .When {""" f1_1: FactOne()"""
          } Then { () =>
          // noop
        }
      }.build

      val rule = {
        //hack
        val ruleField = classOf[RuleImpl].getDeclaredField("rule")
        ruleField.setAccessible(true)
        ruleField.get(builder.knowledgePackages.head.getRules.head).asInstanceOf[org.drools.rule.Rule]
      }
      rule.getName  must_== "rule"
      rule.getSalience.toString  must_== "10" //hack
      rule.getAgendaGroup  must_== "agenda_group"
      rule.getRuleFlowGroup  must_== "ruleflow_group"
      rule.isLockOnActive === true
      rule.isNoLoop === true
    }


    "auto_generated_rule_name" in {
      var rule1_invoked = false
      var rule2_invoked = false


      new Ruleset("niente") {
        Import[FactOne]
        Rule().When {

          """
                 f1_1: FactOne()
          """
        } Then { () =>
          rule1_invoked = true
        }
        Rule().When {
          """
                  f1_1: FactOne()
          """
        } Then { () =>
          rule2_invoked = true
        }
      }.build

      insertAllFactsAndFire
      // Don't care what the name is, just that its unique
      rule1_invoked === true
      rule2_invoked === true
    }

    "multiline_syntax" in {
      var rule1_invoked = false
      var rule2_invoked = false


      new Ruleset("niente") {

        Import[FactOne]
        Import[FactTwo]

        Rule("rule1")
          .When {"""
                 f1_1: FactOne()
                 f2_1: FactTwo()"""
          } Then { (f1_1: FactOne,
                    f2_1: FactTwo) =>
          rule1_invoked = true
        }
      }.build

      insertAllFactsAndFire
      rule1_invoked === true
    }


    "concise_syntax" in {
      var rule1_invoked = false
      var rule2_invoked = false


      new Ruleset( "niente") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule1").When {
          """
             f1_1: FactOne()
             f2_1: FactTwo()
          """
        } Then { (f1_1: FactOne, f2_1: FactTwo) =>
          rule1_invoked = true
        }

        Rule("rule2", salience = 10).When {
          """
                 f1_1: FactOne()
                 f2_1: FactTwo()
          """
        } Then { (f1_1: FactOne, f2_1: FactTwo) =>
          rule2_invoked = true
        }
      }.build


      insertAllFactsAndFire
      rule1_invoked === true
      rule2_invoked === true
    }


    "non_inner_rule_syntax" in {
      var rule1_invoked = false
      var rule2_invoked = false
      var rule3_invoked = false
      var rule4_invoked = false
      val p = new Ruleset("nichts") {
        Import[
          FactOne]
        Import[
          FactTwo]
      }

      p.Rule("p.rule1").When {
        """
        f1_1: FactOne()
        f2_1: FactTwo()
        """
      } Then { (f1_1: FactOne, f2_1: FactTwo) =>
        rule1_invoked = true
      }

      p.Rule("p.rule2", salience = 10).

        When { """
      f1_1: FactOne()
      f2_1: FactTwo()
          """
        } Then { (f1_1: FactOne, f2_1: FactTwo) =>
        rule2_invoked = true
      }

      p.Rule("p.rule3", salience = 10).When {
        'f1_1 := ('FactOne -> nothing)
        'f2_1 := (
          'FactTwo -> nothing)
      }.Then { (f1_1: FactOne, f2_1:
      FactTwo) =>
        rule3_invoked = true
      }


      p.Rule("p.rule4", salience = 10).When {
        'f1_1 := ('FactOne -> nothing)
        'f2_1 := ('FactTwo -> nothing)
      }.Then { (f1_1: FactOne, f2_1: FactTwo) =>
        rule4_invoked = true
      }

      p.build
      insertAllFactsAndFire
      rule1_invoked === true
      rule2_invoked === true
      rule3_invoked === true
      rule4_invoked === true
    }

    "non_inner_rule_using_import_syntax" in {
      var rule1_invoked = false
      var rule2_invoked = false

      val p = new Ruleset("nichts") {
        Import[FactOne]
        Import[FactTwo]
      }

      import p.Rule
      Rule("p.rule1").
        When {  """
        f1_1: FactOne()
        f2_1: FactTwo()
          """
        } Then { (f1_1: FactOne, f2_1: FactTwo) =>

        rule1_invoked = true
      }

      Rule("p.rule2", salience = 10).When {  """
            f1_1: FactOne()
            f2_1: FactTwo()
           """
          } Then { (f1_1: FactOne, f2_1: FactTwo) =>
              rule2_invoked = true
      }

      p.build

      insertAllFactsAndFire
      rule1_invoked === true
      rule2_invoked === true
    }
    //TODO These are probably not testing the default for ScalaPackageBuilder,

    // but there is production code that verifies its working. Maybe completely
    // seperate test is needed...


    "single_package_syntax" in {

      val p = new Ruleset("rien") {
        Import[FactOne]
        Import[FactTwo]

        Rule("rule1")
          .When {
            """ f1_1: FactOne()
                f2_1: FactTwo()"""
          } Then { (f1_1: FactOne, f2_1: FactTwo) =>
          //
        }
      }
      val dkp =
        knowledgePackage() {
          rulesets(p)
        }

      dkp.build

      val kp: KnowledgePackage = dkp.builtKnowledgePackage

      kp.getRules filter (_.getName == "rule1") !== null

    }

    "single_package_subclass_syntax" in {

      class MyRules extends Ruleset {

        Import[FactOne]
        Import[FactTwo]

        Rule("rule1")
          .When {
            """
        f1_1: FactOne()
        f2_1: FactTwo()"""
          } Then { (f1_1: FactOne, f2_1: FactTwo) =>
          //
        }
      }
      val dkp =
        knowledgePackage() {
          rulesets {
            new MyRules
          }
        }

      dkp.build

      val kp: KnowledgePackage = dkp.builtKnowledgePackage
      kp.getRules filter (_.getName == "rule1") !== null

    }
  }
}