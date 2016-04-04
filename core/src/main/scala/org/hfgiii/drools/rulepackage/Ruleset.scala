package org.hfgiii.drools.rulepackage

import org.hfgiii.drools.condition._
import org.hfgiii.drools.knowledgepackage.DroolsPackageElement

import scala.collection.mutable.{LinkedHashMap, MutableList}

class Ruleset(var name:String="") extends  DroolsPackageElement {

  ruleset =>

  name = if (name.length() == 0)
          ruleset.getClass.getSimpleName.split("""\$""")(0)
         else
          name

  val importDescriptors: MutableList[String] = new MutableList[String]

  val rules:    LinkedHashMap[String,Rule]  = LinkedHashMap.empty[String,Rule]
  val queries:  LinkedHashMap[String,Query] = LinkedHashMap.empty[String,Query]

  def Name[T: Manifest] {
    name = manifest[T].erasure.getSimpleName
  }

  def Import[T: Manifest] {
    importDescriptors += ("import "+manifest[T].erasure.getName+"\n")
  }

  private var ruleCount = 0
  private def generateRuleName =
  {
    ruleCount += 1
    this.name + ".rule#" + ruleCount
  }

  private var queryCount = 0
  private def generateQueryName = {
    queryCount += 1
    this.name + ".query#" + queryCount
  }

  implicit def stringToOption(str:String):Option[String] = Option(str)
  implicit def booleanToOption(bool:Boolean):Option[Boolean] = Option(bool)

  case class Rule(name: String = generateRuleName, salience: Int = 0, agendaGroup: Option[String]  = None,
                  var ruleflowGroup: Option[String] = None, lockOnActive: Option[Boolean] = None,
                  noLoop: Option[Boolean] = None)(implicit namespace:LinkedHashMap[Symbol,DroolsCondition]=new LinkedHashMap[Symbol,DroolsCondition]) {

    implicit def rhsToOption(rhs:RHS):Option[RHS]          = Option(rhs)
    implicit def stringToOption(str:String):Option[String] = Option(str)

    //TODO:ruleCount
    //ruleCount += 1
    var lhs: Option[String] = None
    var rhs: Option[RHS]    = None

    ruleset.rules += (name -> this)

    def When(exprs: => DroolsCondition): Rule = {
      exprs

      val condBuf = new StringBuffer

      namespace.foreach(t => condBuf.append( t._2.toMvel + "\n"))
      namespace.clear

      this.lhs = condBuf.toString
    // println(condBuf.toString)
      this
    }

    def When(lhs: String): Rule = {
      this.lhs = Some(lhs)
      this
    }

    def apply(lhs: String): Rule = {
      When(lhs)
    }

    trait RHS

    case class RHS0(rhs: () => Unit) extends RHS
    case class RHS1[T1](rhs: (T1) => Unit) extends RHS
    case class RHS2[T1,T2](rhs: (T1,T2) => Unit) extends RHS
    case class RHS3[T1,T2,T3](rhs: (T1,T2,T3) => Unit) extends RHS
    case class RHS4[T1,T2,T3,T4](rhs: (T1,T2,T3,T4) => Unit) extends RHS
    case class RHS5[T1,T2,T3,T4,T5](rhs: (T1,T2,T3,T4,T5) => Unit) extends RHS
    case class RHS6[T1,T2,T3,T4,T5,T6](rhs: (T1,T2,T3,T4,T5,T6) => Unit) extends RHS
    case class RHS7[T1,T2,T3,T4,T5,T6,T7](rhs: (T1,T2,T3,T4,T5,T6,T7) => Unit) extends RHS
    case class RHS8[T1,T2,T3,T4,T5,T6,T7,T8](rhs: (T1,T2,T3,T4,T5,T6,T7,T8) => Unit) extends RHS
    case class RHS9[T1,T2,T3,T4,T5,T6,T7,T8,T9](rhs: (T1,T2,T3,T4,T5,T6,T7,T8,T9) => Unit) extends RHS
    case class RHS10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](rhs: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) => Unit) extends RHS

    def Then(rhs: () => Unit):Rule = {this.rhs = RHS0(rhs);this}
    def Then[T1](rhs: (T1) => Unit):Rule = {this.rhs = RHS1(rhs);this}
    def Then[T1,T2](rhs: (T1,T2) => Unit):Rule = {this.rhs = RHS2(rhs);this}
    def Then[T1,T2,T3](rhs: (T1,T2,T3) => Unit):Rule =  { this.rhs = RHS3(rhs);this}
    def Then[T1,T2,T3,T4](rhs: (T1,T2,T3,T4) => Unit):Rule =  { this.rhs = RHS4(rhs);this}
    def Then[T1,T2,T3,T4,T5](rhs: (T1,T2,T3,T4,T5) => Unit):Rule =  { this.rhs = RHS5(rhs);this}
    def Then[T1,T2,T3,T4,T5,T6](rhs: (T1,T2,T3,T4,T5,T6) => Unit):Rule =  { this.rhs = RHS6(rhs);this}
    def Then[T1,T2,T3,T4,T5,T6,T7](rhs:  (T1,T2,T3,T4,T5,T6,T7) => Unit):Rule =  { this.rhs = RHS7(rhs);this}
    def Then[T1,T2,T3,T4,T5,T6,T7,T8](rhs: (T1,T2,T3,T4,T5,T6,T7,T8) => Unit):Rule =  { this.rhs = RHS8(rhs);this}
    def Then[T1,T2,T3,T4,T5,T6,T7,T8,T9](rhs: (T1,T2,T3,T4,T5,T6,T7,T8,T9) => Unit):Rule =  { this.rhs = RHS9(rhs);this}
    def Then[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](rhs:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) => Unit):Rule =  { this.rhs = RHS10(rhs);this}


  }

  case class Query (name: String = generateQueryName,qryStr:String)(qry: => DroolsCondition=DroolsNullCondition())(implicit namespace:LinkedHashMap[Symbol,DroolsCondition]=new LinkedHashMap[Symbol,DroolsCondition]) {
      qry

      private val queryBuf = new StringBuffer

      namespace.foreach(t => queryBuf.append( t._2.toMvel + "\n"))
      namespace.clear



      ruleset.queries += (name -> this)

      def query:String = if (queryBuf.toString.length == 0)
                          qryStr
                         else
                          queryBuf.toString

  }
}