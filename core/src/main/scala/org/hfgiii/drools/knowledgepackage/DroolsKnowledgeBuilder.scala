package org.hfgiii.drools.knowledgepackage

import org.hfgiii.drools.ruleflow.Ruleflow
import org.hfgiii.drools.rulepackage.Ruleset

import scala.collection.JavaConversions._
import java.util.{Map => jMap}
import scala.collection.{Map => sMap}
import com.thoughtworks.paranamer.BytecodeReadingParanamer
import org.jbpm.bpmn2.xml.XmlBPMNProcessDumper
import org.jbpm.ruleflow.core.{RuleFlowProcess, RuleFlowProcessFactory}
import org.jbpm.workflow.core.node.{Join, Split}
import org.drools.rule.Declaration
import org.drools.spi.{KnowledgeHelper, Consequence}
import org.drools.WorkingMemory
import collection.mutable.{WeakHashMap,LinkedHashMap,MutableList,Stack}
import org.drools.rule.builder.{RuleBuildContext, ConsequenceBuilder}
import org.drools.builder.impl.KnowledgeBuilderImpl
import org.drools.builder.conf.EvaluatorOption
import org.drools.io.{Resource, ResourceFactory}
import org.drools.definition.KnowledgePackage
import org.drools.builder.{KnowledgeBuilderFactory, ResourceType, KnowledgeBuilder}
import java.io.StringReader
import org.drools.impl.EnvironmentImpl
import org.drools.conf.KnowledgeBaseOption
import org.drools.runtime.conf.KnowledgeSessionOption
import org.drools.{KnowledgeBaseFactory, KnowledgeBaseConfiguration}

import org.drools.{FactException, KnowledgeBase}
import org.drools.event.rule._
import org.drools.runtime.rule.{AgendaFilter, FactHandle}
import org.drools.runtime.{ObjectFilter, KnowledgeSessionConfiguration, StatefulKnowledgeSession}


class DroolsKnowledgeBuilder {

  def warn(message: String) { println("WARN "+message) }

  private[knowledgepackage] val kbuilder = KnowledgeBuilderFactory.newKnowledgeBuilder
  ScalaExtensions.registerScalaEvaluators(kbuilder)

  private val paranamer      = new BytecodeReadingParanamer
  private val paramInfoCache = new WeakHashMap[Class[_], LinkedHashMap[String,Class[_]]]

  private var builderGlobals: Map[String, AnyRef] = Map.empty
  def globals = builderGlobals
  private[knowledgepackage] def addGlobals(globals: Map[String,AnyRef]) {
    globals.foreach { g =>
      if (this.builderGlobals.contains(g._1)) {
        val prevValue = this.builderGlobals(g._1)
        warn("Duplicate global: \n" +
          "  pre: name=%s, value=%s\n".format(g._1, prevValue) +
          " this: name=%s, value=%s\n".format(g._1, g._2))
      }
    }
    this.builderGlobals ++= globals
  }

  private[knowledgepackage] def lookupParameterInfo(functionClazz: Class[_]): LinkedHashMap[String,Class[_]] =
    paramInfoCache.get(functionClazz).fold {

      val applyMethod = functionClazz.getMethods.filter(m => m.getName == "apply" && m.getReturnType == java.lang.Void.TYPE).head

      //THIS INSURES THE PROPER FUNCTIONING OF SCALA For EXPRESSIONS/COMPREHENSIONS IN RULE CONSEQUENCES
      val paramNames  = paranamer.lookupParameterNames(applyMethod).map(_.split("""\$""")(0))
      val paramTypes  = applyMethod.getParameterTypes

      //THIS RELATIVE TYPE-CLUNKINESS IS TO GET THE TYPE INFERENCING CORRECT
      var paramInfo:LinkedHashMap[String,Class[_]] = LinkedHashMap.empty[String,Class[_]]

      (paramNames zip paramTypes) map (e => paramInfo += e)

      paramInfoCache += (functionClazz -> paramInfo)

      paramInfo

    } { identity }

  def knowledgePackages = kbuilder.getKnowledgePackages

  class RuleBuildException(message: String) extends RuntimeException(message)

}

private [knowledgepackage] class KnowledgePackageBuilder(kpack:DroolsKnowledgePackage)(implicit val builder:DroolsKnowledgeBuilder = new DroolsKnowledgeBuilder) {

     import builder._

     private val packageGlobals: LinkedHashMap[String,AnyRef] = kpack.packageGlobals

     private def knowledgePackage: KnowledgePackage = {
       knowledgePackages.foreach( pkg => {
         val pkgName = pkg.getName
         println(pkgName)
       })

       knowledgePackages find (_.getName == kpack.actualName) getOrElse {
                            throw new IllegalStateException("Could not obtain package: name='%s'".format(kpack.actualName))
                         }
     }

     def build() {
         kpack.rulesets.foreach(e =>   buildRuleset(e._2))

         kpack.drlPaths.foreach(buildDRL(_))

         kpack.ruleflows.foreach(e =>  buildRuleflow(e._2))

         kpack.bpmnPaths.foreach(buildBPMN(_))

         kpack.decisionTablePaths.foreach(buildDecisionTable(_))
     }

     private def buildDecisionTable(decisionTable:DroolsDecisionTablePath) {
       decisionTable match {
         case DroolsDecisionTablePath(path,dtType)
            =>  {
                    val  dtConf  = KnowledgeBuilderFactory.newDecisionTableConfiguration
                         dtConf.setInputType(dtType)
                         kbuilder.add(ResourceFactory.newClassPathResource(path), ResourceType.DTABLE,dtConf)
                           if (kbuilder.hasErrors) throw new RuleBuildException(kbuilder.getErrors.mkString(","))
                }
         case _
            =>
       }
     }

     private def buildDRL(drl:DroolsDRLPath) {
        drl match {
          case DroolsDRLPath(path)
            => {
                  kbuilder.add(ResourceFactory.newFileResource(path), ResourceType.DRL)
                    if (kbuilder.hasErrors) throw new RuleBuildException(kbuilder.getErrors.mkString(","))
               }
           case _
              =>
        }
     }

     private def buildBPMN(bpmn:DroolsBPMNPath) {
         bpmn match {
           case DroolsBPMNPath(path)
              => {
                    kbuilder.add(ResourceFactory.newClassPathResource(path), ResourceType.BPMN2)
                      if (kbuilder.hasErrors) throw new RuleBuildException(kbuilder.getErrors.mkString(","))
                 }
           case _
              =>
         }
     }

     private def buildRuleflow(ruleflow:Ruleflow) {
       val builder = RuleflowBuilder(ruleflow)
       builder.build()
      // println(XmlBPMNProcessDumper.INSTANCE.dump(builder.process))
       kbuilder.add(ResourceFactory.newByteArrayResource(XmlBPMNProcessDumper.INSTANCE.dump(builder.process).getBytes()), ResourceType.BPMN2)
          if (kbuilder.hasErrors) throw new RuleBuildException(kbuilder.getErrors.mkString(","))


     }

     private def buildRuleset(ruleset:Ruleset) {
       implicit val importDescriptors = ruleset.importDescriptors
       ruleset.rules.foreach(e => buildRule(e._2))
       ruleset.queries.foreach(e => buildQuery(e._2))
     }

     private def buildRule(rule :Ruleset#Rule)(implicit importDescriptors:MutableList[String]) {
         rule.rhs match {
           case Some(rule.RHS0(f0))   => new RuleBuilder0(rule,f0).build
           case Some(rule.RHS1(f1))   => new RuleBuilder1(rule,f1).build
           case Some(rule.RHS2(f2))   => new RuleBuilder2(rule,f2).build
           case Some(rule.RHS3(f3))   => new RuleBuilder3(rule,f3).build
           case Some(rule.RHS4(f4))   => new RuleBuilder4(rule,f4).build
           case Some(rule.RHS5(f5))   => new RuleBuilder5(rule,f5).build
           case Some(rule.RHS6(f6))   => new RuleBuilder6(rule,f6).build
           case Some(rule.RHS7(f7))   => new RuleBuilder7(rule,f7).build
           case Some(rule.RHS8(f8))   => new RuleBuilder8(rule,f8).build
           case Some(rule.RHS9(f9))   => new RuleBuilder9(rule,f9).build
           case Some(rule.RHS10(f10)) => new RuleBuilder10(rule,f10).build

           case None =>
         }
    }

    private def buildQuery(query :Ruleset#Query)(implicit importDescriptors:MutableList[String]) {
         new QueryBuilder(query).build
    }

   private[knowledgepackage] case class RuleflowBuilder(ruleflow:Ruleflow) {

      private val builder      = new RuleflowProcessBuilder(RuleFlowProcessFactory.createProcess(ruleflow.name))
      private var elementIndex:Long = 0

      private def currentIndex(re:Ruleflow#DroolsRuleflowElement):Long = {
         elementIndex += 1

         re.number = elementIndex

         elementIndex
      }


      def process:RuleFlowProcess = builder.getProcess

      def build() {

         builder
		    .name(ruleflow.name)
		    .version("1.0")
        .packageName("org.drools.bpmn2")

        buildImports(ruleflow.importDescriptors)

        buildStart(ruleflow.start)

        ruleflow.ruleflowGroups.foreach(e => buildRuleflowGroup(e._2))

        buildEnd(ruleflow.end)

        ruleflow.ruleflowGateways.foreach(e => buildGateway(e._2))

        ruleflow.ruleflowSequences.foreach(buildSequence(_))

        builder.validate

      }

      private def buildImports(imports:MutableList[String]) {
        imports.isEmpty match {
          case false => builder.imports(imports.toArray[String])
          case true  =>
        }
      }

      private def buildStart(start:Ruleflow#Start) {
         builder
		    .startNode(currentIndex(start))
        .name(start.name)
        .done

      }

      private def buildEnd(end:Ruleflow#End) {
         builder
		    .endNode(currentIndex(end))
        .name(end.name)
        .done
      }

      private def buildRuleflowGroup(group:Ruleflow#DroolsRuleflowGroup) {
        builder
		   .ruleSetNode(currentIndex(group))
		       .name(group.name)
		       .ruleFlowGroup(group.name)
        .done
      }

      private def buildGateway(gateway:Ruleflow#DroolsRuleflowGateway) {

         def exclusiveGatewayBuild(xgateway:ruleflow.DroolsRuleflowGateway,splitType:Int) {

                   val snb =
                       builder
                      .splitNode(currentIndex(xgateway))
                      .`type`(splitType)
                      .name(xgateway.name)

                   xgateway.legs match {
                        case Left(_)     =>
                        case Right(seqs) => seqs.foreach(tpl => {
                            snb.constraint(tpl._1.number,
                                           xgateway.number + "_" + tpl._1.number,
                                           "rule",
                                           "mvel",
                                           tpl._2.toMvel)
                            })

                            snb.done

                            seqs.foreach(tpl => ruleflow.ruleflowSequences += ruleflow.DroolsRuleflowSequence(xgateway,tpl._1))
                   }
         }

         def convergingGatewayBuild(convergingGateway:ruleflow.DroolsRuleflowGateway,joinType:Int) {
                   builder
                  .joinNode(currentIndex(convergingGateway))
                  .`type`(joinType)
                  .name(convergingGateway.name)
                  .done

                   convergingGateway.legs match {
                        case Left(seqs) => seqs.foreach( dre => ruleflow.ruleflowSequences += ruleflow.DroolsRuleflowSequence(dre,convergingGateway))
                        case Right(_)   =>
                   }
         }

         gateway match {
           case andd : ruleflow.ANDDivergingGateway
               => {
                     builder
                     .splitNode(currentIndex(andd))
                     .`type`(Split.TYPE_AND)
                     .name(andd.name)
                     .done

                      andd.legs match {
                        case Left(_)     =>
                        case Right(seqs) => seqs.foreach(tpl => ruleflow.ruleflowSequences += ruleflow.DroolsRuleflowSequence(andd,tpl._1))
                        case _           =>
                      }
                   }

           case ord  : ruleflow.ORDivergingGateway
               =>  exclusiveGatewayBuild(ord,Split.TYPE_OR)

           case xord : ruleflow.XORDivergingGateway
               =>  exclusiveGatewayBuild(xord,Split.TYPE_XOR)

           case xorc : ruleflow.XORConvergingGateway
               =>  convergingGatewayBuild(xorc,Join.TYPE_XOR)

           case andc : ruleflow.ANDConvergingGateway
               =>  convergingGatewayBuild(andc,Join.TYPE_AND)

           case nullc : ruleflow.NullGateway
               =>
           case _
               =>
         }
      }

      private def buildSequence(sequence:Ruleflow#DroolsRuleflowSequence) {

        import sequence._

        builder
          .connection(from.number,to.number)


      }
   }

  private[knowledgepackage] class QueryBuilder(descriptor: Ruleset#Query)(implicit importDescriptors:MutableList[String])  {

  private def packagedDrl: String = {
      """
      package %s
      %s
      query "%s"
        %s
      end
      """.format(kpack.actualName,
                 if (importDescriptors.isEmpty) "" else importDescriptors reduceLeft (_ ++ _),
                 descriptor.name,
                 descriptor.query)
   }

   def build {
      try {
     //   println(packagedDrl)

        kbuilder.add(ResourceFactory.newReaderResource(new StringReader(packagedDrl)), ResourceType.DRL)

        if (kbuilder.hasErrors) throw new RuleBuildException(kbuilder.getErrors.mkString(","))

      } finally {}
    }
  }

  private[knowledgepackage] abstract class RuleBuilder(descriptor: Ruleset#Rule)(implicit importDescriptors:MutableList[String]) {

    private def attribute(quote: Char)(name: String, value: Option[Any]) = value match {
      case Some(value) => name + " " + quote + value + quote
      case None => ""
    }
    private def stringAttribute = attribute(quote = '"') _
    private def valueAttribute = attribute(quote = ' ') _

    private def globalDrl(global: (String, AnyRef)) = "global %s %s\n".format(global._2.getClass.getName, global._1)

    private def packagedDrl: String = {
      """
      package %s
      %s
      import scala.Option
      global scala.None$ None
      %s
      rule "%s" dialect "embedded-scala"
        salience %d
        %s //agenda-group
        %s //ruleflow-group
        %s //no-loop
        %s //lock-on-active
      when
        %s
      then
        // placeholder for parser
      end
      """.format(kpack.actualName,
                 if (importDescriptors.isEmpty) "" else importDescriptors reduceLeft (_ ++ _),
                 if (packageGlobals.isEmpty) "" else packageGlobals map (globalDrl) reduceLeft (_ ++ _),
                 descriptor.name,
                 descriptor.salience,
                 stringAttribute("agenda-group", descriptor.agendaGroup),
                 stringAttribute("ruleflow-group", descriptor.ruleflowGroup),
                 valueAttribute("no-loop", descriptor.noLoop),
                 valueAttribute("lock-on-active", descriptor.lockOnActive),
                 descriptor.lhs.get)
    }

    def consequence(declaraions: jMap[String,Declaration]): Consequence

    def build {
      ScalaConsequenceBuilder.builderStack.push(this)
      try {
       // println(packagedDrl)
        kbuilder.add(ResourceFactory.newReaderResource(new StringReader(packagedDrl)), ResourceType.DRL)
        if (kbuilder.hasErrors) throw new RuleBuildException(kbuilder.getErrors.mkString(","))
       // addGlobals(packageGlobals.asInstanceOf[sMap[String,AnyRef]])
      } finally {
        ScalaConsequenceBuilder.builderStack.pop
      }
    }
    }
    private abstract class TypedConsequence(declarations: sMap[String,Declaration], rhsClazz: Class[_]) extends Consequence {

      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef])

    // Type check function parameters against declarations
      lookupParameterInfo(rhsClazz).foreach { paramInfo => paramInfo._2 match {
        case paramClazz if paramClazz.isAssignableFrom(classOf[KnowledgeHelper]) => {} //ok
        case paramClazz if paramClazz.isAssignableFrom(classOf[WorkingMemory])   => {} //ok

        case paramClazz => {
          val declaration = declarations.get(paramInfo._1) getOrElse {
                 throw new RuleBuildException("Consequence parameter '%s: %s' does not match any fact identifiers (missing Import?)"
                 .format(paramInfo._1, paramInfo._2.getName))
              }

          val factClazz = declaration.getExtractor.getExtractToClass

          if (!paramClazz.isAssignableFrom(factClazz)) {
            throw new RuleBuildException("Consequence parameter '%s: %s' not assignable from Fact '%s: %s'"
              .format(paramInfo._1, paramInfo._2.getName, declaration.getIdentifier, factClazz.getName))
          }
        }
    }}


    def evaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory) {
      def facts: Seq[AnyRef] = //TODO Could this be (partially) cached somehow?
        Seq.empty[AnyRef] ++ lookupParameterInfo(rhsClazz).map { paramType => paramType._2 match {
          case pclazz if pclazz.isAssignableFrom(classOf[KnowledgeHelper]) => knowledgeHelper
          case pclazz if pclazz.isAssignableFrom(classOf[WorkingMemory]) => workingMemory
          case _ => knowledgeHelper.getTuple.get(declarations(paramType._1)).getObject
        }}

      val factoto = facts
      doEvaluate(knowledgeHelper, workingMemory, factoto)
    }

    def getName = "<function>"

  }

  //TODO Find some way to kill this smelly boilerplate (without reflection)

  private class   RuleBuilder0(ruleDescriptor: Ruleset#Rule, rhs: () => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs()
      }
    }
  }

  private class   RuleBuilder1[+T1](ruleDescriptor: Ruleset#Rule, rhs: (T1) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1])
      }
    }
  }

  private class   RuleBuilder2[+T1,+T2](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2])
      }
    }
  }

  private class   RuleBuilder3[+T1,+T2,+T3](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2,T3) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2], facts(2).asInstanceOf[T3])
      }
    }
  }

  private class   RuleBuilder4[+T1,+T2,+T3,+T4](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2,T3,T4) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2], facts(2).asInstanceOf[T3], facts(3).asInstanceOf[T4])
      }
    }
  }

  private class   RuleBuilder5[+T1,+T2,+T3,+T4,+T5](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2,T3,T4,T5) => Unit) (implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2], facts(2).asInstanceOf[T3], facts(3).asInstanceOf[T4], facts(4).asInstanceOf[T5])
      }
    }
  }

  private class   RuleBuilder6[+T1,+T2,+T3,+T4,+T5,+T6](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2,T3,T4,T5,T6) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2], facts(2).asInstanceOf[T3], facts(3).asInstanceOf[T4], facts(4).asInstanceOf[T5],
            facts(5).asInstanceOf[T6])
      }
    }
  }

  private class   RuleBuilder7[+T1,+T2,+T3,+T4,+T5,+T6,+T7](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2,T3,T4,T5,T6,T7) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2], facts(2).asInstanceOf[T3], facts(3).asInstanceOf[T4], facts(4).asInstanceOf[T5],
            facts(5).asInstanceOf[T6], facts(6).asInstanceOf[T7])
      }
    }
  }

  private class   RuleBuilder8[+T1,+T2,+T3,+T4,+T5,+T6,+T7,+T8](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2,T3,T4,T5,T6,T7,T8) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2], facts(2).asInstanceOf[T3], facts(3).asInstanceOf[T4], facts(4).asInstanceOf[T5],
            facts(5).asInstanceOf[T6], facts(6).asInstanceOf[T7], facts(7).asInstanceOf[T8])
      }
    }
  }

  private class   RuleBuilder9[+T1,+T2,+T3,+T4,+T5,+T6,+T7,+T8,+T9](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2,T3,T4,T5,T6,T7,T8,T9) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2], facts(2).asInstanceOf[T3], facts(3).asInstanceOf[T4], facts(4).asInstanceOf[T5],
            facts(5).asInstanceOf[T6], facts(6).asInstanceOf[T7], facts(7).asInstanceOf[T8], facts(8).asInstanceOf[T9])
      }
    }
  }

  private class   RuleBuilder10[+T1,+T2,+T3,+T4,+T5,+T6,+T7,+T8,+T9,+T10](ruleDescriptor: Ruleset#Rule, rhs: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) => Unit)(implicit importDescriptors:MutableList[String])
          extends RuleBuilder(ruleDescriptor) {
    def consequence(declarations: jMap[String,Declaration]) = new TypedConsequence(declarations, rhs.getClass) {
      def doEvaluate(knowledgeHelper: KnowledgeHelper, workingMemory: WorkingMemory, facts: Seq[AnyRef]) {
        rhs(facts(0).asInstanceOf[T1], facts(1).asInstanceOf[T2], facts(2).asInstanceOf[T3], facts(3).asInstanceOf[T4], facts(4).asInstanceOf[T5],
            facts(5).asInstanceOf[T6], facts(6).asInstanceOf[T7], facts(7).asInstanceOf[T8], facts(8).asInstanceOf[T9], facts(9).asInstanceOf[T10])
      }
    }
  }

}

private[knowledgepackage] object ScalaConsequenceBuilder extends ConsequenceBuilder {

  private[knowledgepackage] val builderStack = new Stack[KnowledgePackageBuilder#RuleBuilder]

  def build(context: RuleBuildContext, name: String) {
    context.getBuildStack.push(context.getRule.getLhs)
    try {
      val declarations = context.getDeclarationResolver.getDeclarations(context.getRule)
      val builder = builderStack.top
      context.getRule.setConsequence(builder.consequence(declarations))
    } finally {
      context.getBuildStack.pop
    }
  }
}

private[knowledgepackage] object ScalaExtensions {

  //TODO This doesn't seem to work.
  val scalaDrls = Seq("org/hfgiii/drools/knowledgepackage/drools_scala_predef.drl")

  def defineScalaDrlElements(kbuilder: KnowledgeBuilder) {
    scalaDrls.foreach { filename =>
       kbuilder.add(ResourceFactory.newClassPathResource(filename), ResourceType.DRL)
    }
  }

  def setScalaGlobals(ksession: StatefulKnowledgeSession) {
   // ksession.setGlobal("None", None)
  }

  /**
   * Evaluators are designed to be added via the 'drools.default.packagebuilder.conf' property file.
   * However, the order that evaluators are added by the runtime is indeterminate. which means that the
   * scala evalutors -- which override drools provided evaluators -- may get installed before the
   * drools evalators and hence be overriden.
   */
  def registerScalaEvaluators(kbuilder: KnowledgeBuilder) {
    val kbuilderConfig = kbuilder.asInstanceOf[KnowledgeBuilderImpl].getPackageBuilder.getPackageBuilderConfiguration
    val chainedProperties = kbuilderConfig.getChainedProperties
    val evaluatorRegistry = kbuilderConfig.getEvaluatorRegistry

    val scalaEntries = new java.util.HashMap[String, String]
    chainedProperties.mapStartsWith(scalaEntries, EvaluatorOption.PROPERTY_NAME+"scala_", true)
    for (className <- scalaEntries.values) {
      evaluatorRegistry.addEvaluatorDefinition(className)
    }
  }

}

object DroolsKnowledgeBuilder {

  class RichKnowledgeBase(val kbase: KnowledgeBase) {

    def addFilesDrls(drlFilenames: Iterable[String]) {
      kbase.addKnowledgePackages(buildFileDrls(drlFilenames))
    }
    def addStringDrls(drlStrings: Iterable[String]) {
      kbase.addKnowledgePackages(buildStringDrls(drlStrings))
    }

    def newScalaStatefulKnowledgeSession(ksessionConfig: KnowledgeSessionConfiguration, globals: Map[String, AnyRef] = Map.empty) = {
      val ksession: StatefulKnowledgeSession = kbase.newStatefulKnowledgeSession(ksessionConfig, null)
      ScalaExtensions.setScalaGlobals(ksession)
      ksession.addGlobals(globals)
      ksession
    }
  }

  implicit def enrichKnowledgeBase(kbase: KnowledgeBase): RichKnowledgeBase = new RichKnowledgeBase(kbase)
  implicit def derichKnowledgeBase(rkbase: RichKnowledgeBase): KnowledgeBase = rkbase.kbase
   class RichStatefulKnowledgeSession(val session: StatefulKnowledgeSession) {

    def knowledgeBase = session.getKnowledgeBase

    def addPackages(drlFilenames: Iterable[String]) {
      knowledgeBase.addKnowledgePackages(buildFileDrls(drlFilenames))
    }

    def addGlobal(identifier: String, value: AnyRef) =
      session.getGlobal(identifier) match {
        case currentValue: AnyRef => throw new IllegalArgumentException("Global already defined: (%s -> %s)".format(identifier, currentValue))
        case _ => session.setGlobal(identifier, value)
      }

    def addGlobals(globals: Map[String,AnyRef]) {
      globals.foreach { global => addGlobal(global._1, global._2) }
    }

    def handleOf(fact: AnyRef): FactHandle = {
      session.getFactHandle(fact) match {
        case handle: FactHandle => handle
        case null => throw new FactException("Fact handle not found")
      }
    }

    def update(oldFact: Object, newFact: Object) {
      session.update(handleOf(oldFact), newFact)
    }

    def insertOrUpdate (fact: AnyRef): FactHandle = {
      session.getFactHandle(fact) match {
        case handle: FactHandle => {session insert handle; handle}
        case fact => session insert fact
      }
    }

    def retractFact (fact: AnyRef) {
      session.getFactHandle(fact) match {
        case handle: FactHandle => session retract handle
        case null => throw new FactException("Fact handle not found: " + fact)
      }
    }

    def facts[T: Manifest]: Set[T] = {
      val filter = new ObjectFilter() {
        def accept(obj: AnyRef) = manifest[T].erasure.isAssignableFrom(obj.getClass)
      }
      Set.empty[T] ++ (for (obj <- session.getObjects(filter)) yield obj.asInstanceOf[T])
    }

    def onInserted[T](f : T => Any)(implicit m: Manifest[T]) {
      session addEventListener new DefaultWorkingMemoryEventListener {
        override def objectInserted(e: ObjectInsertedEvent) =
          if (m.erasure.isAssignableFrom(e.getObject.getClass)) {
            f(e.getObject.asInstanceOf[T])
          }
      }
    }
    def onInserted(f : ObjectInsertedEvent => Unit) {
      session addEventListener new DefaultWorkingMemoryEventListener {
        override def objectInserted(e: ObjectInsertedEvent) = f(e)
      }
    }

    def onRetracted[T](f : T => Any)(implicit m: Manifest[T]) {
      session addEventListener new DefaultWorkingMemoryEventListener {
        override def objectRetracted(e: ObjectRetractedEvent) =
          if (m.erasure.isAssignableFrom(e.getOldObject.getClass)) f(e.getOldObject.asInstanceOf[T])
      }
    }
    def onRetracted(f : ObjectRetractedEvent => Unit) {
      session addEventListener new DefaultWorkingMemoryEventListener {
        override def objectRetracted(e: ObjectRetractedEvent) = f(e)
      }
    }

    def onUpdated[T](f : T => Any)(implicit m: Manifest[T]) {
      session addEventListener new DefaultWorkingMemoryEventListener {
        override def objectUpdated(e: ObjectUpdatedEvent) =
          if (m.erasure.isAssignableFrom(e.getObject.getClass)) f(e.getObject.asInstanceOf[T])
      }
    }
    def onUpdated(f : ObjectUpdatedEvent => Unit) {
      session addEventListener new DefaultWorkingMemoryEventListener {
        override def objectUpdated(e: ObjectUpdatedEvent) = f(e)
      }
    }

    def onAfterActivationFired(f : AfterActivationFiredEvent => Unit) {
      session addEventListener new DefaultAgendaEventListener {
        override def afterActivationFired(e: AfterActivationFiredEvent) = f(e)
      }
    }
    def onBeforeActivationFired(f : BeforeActivationFiredEvent => Unit) {
      session addEventListener new DefaultAgendaEventListener {
        override def beforeActivationFired(e: BeforeActivationFiredEvent) = f(e)
      }
    }

    def fire() = session.fireAllRules()
    def fire(max: Int) = session.fireAllRules(max)
    def fire(filter: AgendaFilter) = session.fireAllRules(filter)

  }

  implicit def enrichSession(ksession: StatefulKnowledgeSession): RichStatefulKnowledgeSession = new RichStatefulKnowledgeSession(ksession)
  implicit def derichSession(rksession: RichStatefulKnowledgeSession): StatefulKnowledgeSession = rksession.session

  def buildDrls(newResource: String => Resource, drls: Iterable[String]): Iterable[KnowledgePackage] = {
    val kbuilder = KnowledgeBuilderFactory.newKnowledgeBuilder
    ScalaExtensions.registerScalaEvaluators(kbuilder)

    ScalaExtensions.defineScalaDrlElements(kbuilder)
    drls foreach { drl => kbuilder.add(newResource(drl), ResourceType.DRL) }

    if (kbuilder.hasErrors) throw new RuntimeException(kbuilder.getErrors.mkString(","))
    kbuilder.getKnowledgePackages
  }

  def buildFileDrls(fileDrls: Iterable[String]): Iterable[KnowledgePackage] = buildDrls(ResourceFactory.newClassPathResource, fileDrls)

  def buildStringDrls(stringDrls: Iterable[String]): Iterable[KnowledgePackage] = {
    def newStringResource(drl: String) = ResourceFactory.newReaderResource(new StringReader(drl))
    buildDrls(newStringResource, stringDrls)
  }

  val emptyEnvironment = new EnvironmentImpl

  implicit def kbaseOptionsToKnowledgeBaseConfiguration(kbaseOptions: Seq[KnowledgeBaseOption]): KnowledgeBaseConfiguration = {
    val kbaseConfig = KnowledgeBaseFactory.newKnowledgeBaseConfiguration()
    kbaseOptions.foreach { kbaseConfig.setOption(_) }
    kbaseConfig
  }

  implicit def ksessionOptionsToKnowledgeSessionConfiguration(ksessionOptions: Seq[KnowledgeSessionOption]): KnowledgeSessionConfiguration = {
    val ksessionConfig = KnowledgeBaseFactory.newKnowledgeSessionConfiguration
    ksessionOptions.foreach { ksessionConfig.setOption(_) }
    ksessionConfig
  }

  def newKnowledgeBase(knowledgePackages: Iterable[KnowledgePackage], kbaseConfig: KnowledgeBaseConfiguration): KnowledgeBase = {
    val kbase = KnowledgeBaseFactory.newKnowledgeBase(kbaseConfig)
    kbase.addKnowledgePackages(knowledgePackages)
    kbase
  }

  def newBuilder = {
    new DroolsKnowledgeBuilder
  }
}