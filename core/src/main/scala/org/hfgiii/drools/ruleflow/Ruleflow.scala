package org.hfgiii.drools.ruleflow

import DroolsRuleflowGatewayLogicType._
import DroolsRuleflowGatewayType._
import collection.mutable.LinkedHashMap._
import collection.mutable.{MutableList, LinkedHashMap}
import org.hfgiii.drools.knowledgepackage.DroolsPackageElement
import org.hfgiii.drools.rulepackage.Ruleset
import org.hfgiii.drools.condition._

object  Ruleflow {
   implicit def optionToStart(os:Option[Ruleflow#Start]):Ruleflow#Start = os.get
   implicit def optionToEnd(os:Option[Ruleflow#End]):Ruleflow#End = os.get
}

case class Ruleflow(name:String) extends DroolsPackageElement {

  ruleflow =>

  val ruleflowGroups:LinkedHashMap[String,DroolsRuleflowGroup]     = empty[String,DroolsRuleflowGroup]
  val ruleflowGateways:LinkedHashMap[String,DroolsRuleflowGateway] = empty[String,DroolsRuleflowGateway]
  val ruleflowEvents:LinkedHashMap[String,DroolsRuleflowEvent]     = empty[String,DroolsRuleflowEvent]
  val ruleflowSequences:MutableList[DroolsRuleflowSequence]  = new MutableList[DroolsRuleflowSequence]()


   var start:Option[Start]             = None
   var end:Option[End]                 = None
   val importDescriptors: MutableList[String] = new MutableList[String]

   def Import[T: Manifest] {
     importDescriptors += manifest[T].erasure.getName
   }

   def ruleflowgroup(name:String)(seqs: => Ruleset):DroolsRuleflowGroup = {
      val rfg = DroolsRuleflowGroup(name)(seqs)

     ruleflow.ruleflowGroups += (name -> rfg)

     rfg
   }

   def divergingGateway(name:String,gtype:DroolsRuleflowGatewayLogicType = UNSUPPORTED)(seqs: (DroolsRuleflowElement,DroolsCondition)* ):DroolsRuleflowGateway =
      gtype match {
        case XOR => XORDivergingGateway(name,seqs:_*)
        case OR  => ORDivergingGateway(name,seqs:_*)
        case AND => seqs match {
          case x: Seq[(DroolsRuleflowElement,DroolsNullCondition)]
                 => ANDDivergingGateway(name,x:_*)
          case _ => NullGateway()
        }
      }


   def convergingGateway(name:String,gtype:DroolsRuleflowGatewayLogicType = UNSUPPORTED)(seqs: DroolsRuleflowElement*):DroolsRuleflowGateway =
      gtype match {
        case XOR => XORConvergingGateway(name,seqs:_*)
        case AND => ANDConvergingGateway(name,seqs:_*)
        case _   => NullGateway()
      }



   def emptyDivergingGateway(name:String,gtype:DroolsRuleflowGatewayLogicType = UNSUPPORTED):DroolsRuleflowGateway =
      gtype match {
        case XOR => XORDivergingGateway(name)
        case OR  => ORDivergingGateway(name)
        case AND => ANDDivergingGateway(name)
        case _   => NullGateway()
      }


   def emptyConvergingGateway(name:String,gtype:DroolsRuleflowGatewayLogicType = UNSUPPORTED):DroolsRuleflowGateway =
      gtype match {
        case XOR => XORConvergingGateway(name)
        case AND => ANDConvergingGateway(name)
        case _   => NullGateway()
      }


   def start(name:String):Start = Start(name)
   def end(name:String):End     = End(name)


  case class DroolsBPMNPath(path:String) extends  DroolsPackageElement

  trait DroolsRuleflowElement  {
    var number:Long = 0

    def >=> (that:DroolsRuleflowGroup): DroolsRuleflowSequence = {

      val sequence  =  DroolsRuleflowSequence(this,that)

      ruleflow.ruleflowSequences   += sequence

      sequence
    }

    def >=> (that:DroolsRuleflowGateway): DroolsRuleflowSequence = {
      val sequence  =  DroolsRuleflowSequence(this,that)

      ruleflow.ruleflowSequences   += sequence

      sequence
    }

    def >=> (that:DroolsRuleflowSequence): DroolsRuleflowSequence = {
      val sequence  =  DroolsRuleflowSequence(this,that)

      ruleflow.ruleflowSequences   += sequence

      sequence
    }

    def >=> (that:DroolsRuleflowEvent): DroolsRuleflowSequence = {
      val sequence  =  DroolsRuleflowSequence(this,that)

      ruleflow.ruleflowSequences   += sequence

      sequence
    }
  }

  case class DroolsNullRuleflowElement()                         extends DroolsRuleflowElement
  case class DroolsRuleflowGroup(name:String)(rulesets:Ruleset*) extends DroolsRuleflowElement  {
    for {
      ruleset <- rulesets
      rule    <- ruleset.rules.values
    } {  rule.ruleflowGroup = Option(name) }
  }

  abstract class DroolsRuleflowEvent(name:String)                extends  DroolsRuleflowElement {
     ruleflow.ruleflowEvents += (name -> this)
  }

  case class Start(name:String) extends DroolsRuleflowEvent(name+"Start")  {
     ruleflow.start           = Some(this)
  }

  case class End(name:String) extends  DroolsRuleflowEvent(name+"End") {
    ruleflow.end = Some(this)

    override def >=> (that:DroolsRuleflowGroup): DroolsRuleflowSequence = {
      DroolsRuleflowSequence(DroolsNullRuleflowElement(),DroolsNullRuleflowElement())
    }

    override def >=> (that:DroolsRuleflowSequence): DroolsRuleflowSequence = {
      DroolsRuleflowSequence(DroolsNullRuleflowElement(),DroolsNullRuleflowElement())
    }

    override def >=> (that:DroolsRuleflowGateway): DroolsRuleflowSequence = {
      DroolsRuleflowSequence(DroolsNullRuleflowElement(),DroolsNullRuleflowElement())
    }

    override def >=> (that:DroolsRuleflowEvent): DroolsRuleflowSequence = {
      DroolsRuleflowSequence(DroolsNullRuleflowElement(),DroolsNullRuleflowElement())
    }
  }

  case class DroolsRuleflowSequence(val from:DroolsRuleflowElement,val to:DroolsRuleflowElement)     extends DroolsRuleflowElement

  sealed trait DroolsRuleflowGateway extends DroolsRuleflowElement {
    protected var flegs:Either[Seq[DroolsRuleflowElement],Seq[(DroolsRuleflowElement,DroolsCondition)]] = Right(Seq.empty[(DroolsRuleflowElement,DroolsCondition)])
    def name:String
    def legs:Either[Seq[DroolsRuleflowElement],Seq[(DroolsRuleflowElement,DroolsCondition)]] = flegs
    def gType:DroolsRuleflowGatewayType       = UNKNOWN
    def gLtype:DroolsRuleflowGatewayLogicType = UNSUPPORTED

    private var addendum:Int = 0

    private def newName:String = {
      addendum += 1

      name + "_" + addendum
    }

    ruleflow.ruleflowGateways   += (name -> this)

    def -<  (lgs:(DroolsRuleflowElement,DroolsCondition)*):XORDivergingGateway  =
      legs match {
        case Right(seqs) if (gType == DIVERGING && gLtype == XOR)
                           => flegs = Right(seqs ++ lgs) ; this.asInstanceOf[XORDivergingGateway]
        case _             => this.asInstanceOf[XORDivergingGateway]
      }

    def -|< (lgs:(DroolsRuleflowElement,DroolsCondition)*):ORDivergingGateway =
      legs match {
        case Right(seqs) if (gType == DIVERGING && gLtype == OR)
                           => flegs = Right(seqs ++ lgs) ; this.asInstanceOf[ORDivergingGateway]
        case _             => this.asInstanceOf[ORDivergingGateway]
      }

    def -&< (lgs:(DroolsRuleflowElement,DroolsNullCondition)*):ANDDivergingGateway =
      legs match {
        case Right(seqs) if (gType == DIVERGING && gLtype == AND)
                         => seqs match {
                                case  sqs:Seq[(DroolsRuleflowElement,DroolsNullCondition)]
                                  =>  flegs = Right(sqs ++ lgs) ; this.asInstanceOf[ANDDivergingGateway]
                                case _
                                  => this.asInstanceOf[ANDDivergingGateway]
                            }

        case _           => this.asInstanceOf[ANDDivergingGateway]
      }

    def >&- (lgs:DroolsRuleflowElement*):ANDConvergingGateway   =
      legs match {
        case Left(seqs)  if (gType == CONVERGING && gLtype == AND)
                           => flegs = Left(seqs ++ lgs) ; this.asInstanceOf[ANDConvergingGateway]
        case Right(seqs)   => this.asInstanceOf[ANDConvergingGateway]
      }

    def >-  (lgs:DroolsRuleflowElement*):XORConvergingGateway    =
      legs match {
        case Left(seqs)  if (gType == CONVERGING && gLtype == XOR)
                           => flegs = Left(seqs ++ lgs) ; this.asInstanceOf[XORConvergingGateway]
        case Right(seqs)   => this.asInstanceOf[XORConvergingGateway]
      }
  }

  case class   ANDDivergingGateway(anddname:String,lgs:(DroolsRuleflowElement,DroolsNullCondition)*)
       extends DroolsRuleflowGateway  {
       def name:String = anddname
       override def gType:DroolsRuleflowGatewayType       = DIVERGING
       override def gLtype:DroolsRuleflowGatewayLogicType = AND

       flegs = Right(lgs)

       val sequences:Seq[DroolsRuleflowSequence] =  lgs.map(tpl => DroolsRuleflowSequence(this,tpl._1))
  }

  case class   ORDivergingGateway(ordname:String,lgs:(DroolsRuleflowElement,DroolsCondition)*)
       extends DroolsRuleflowGateway  {
       def name:String = ordname
       override def gType:DroolsRuleflowGatewayType       = DIVERGING
       override def gLtype:DroolsRuleflowGatewayLogicType = OR

       flegs = Right(lgs)

  }

  case class   XORDivergingGateway(xordname:String,lgs:(DroolsRuleflowElement,DroolsCondition)*)
       extends DroolsRuleflowGateway {
       def name:String = xordname
       override def gType:DroolsRuleflowGatewayType       = DIVERGING
       override def gLtype:DroolsRuleflowGatewayLogicType = XOR

       flegs = Right(lgs)

  }


  case class   XORConvergingGateway(xorcname:String,lgs:DroolsRuleflowElement*)
       extends DroolsRuleflowGateway {
       def name:String = xorcname
       override def gType:DroolsRuleflowGatewayType       = CONVERGING
       override def gLtype:DroolsRuleflowGatewayLogicType = XOR

       flegs = Left(lgs)

  }


  case class   ANDConvergingGateway(andcname:String,lgs:DroolsRuleflowElement*)
       extends DroolsRuleflowGateway   {
       def name:String = andcname
       override def gType:DroolsRuleflowGatewayType       = CONVERGING
       override def gLtype:DroolsRuleflowGatewayLogicType = AND

       flegs = Left(lgs)

  }

  case class   NullGateway()
       extends DroolsRuleflowGateway {
        def name:String = "Null"
        override def gType:DroolsRuleflowGatewayType       = NULL
        override def gLtype:DroolsRuleflowGatewayLogicType = NONE

        flegs  = Left(Seq(DroolsNullRuleflowElement()))

  }

}
