package org.hfgiii.drools.condition

import collection.mutable.LinkedHashMap

sealed trait DroolsCondition {
  def toMvel():String
}

case class DroolsNullCondition() extends DroolsCondition {
  def toMvel():String = ""
}

trait DroolsRelational extends DroolsCondition  {
  def lhs:DroolsCondition
  def rhs:DroolsCondition
  def && (rexpr:DroolsRelational) = DroolsAnd(this,rexpr)
  def && (bexpr:DroolsBoolean)    = DroolsAnd(this,bexpr)
  def || (rexpr:DroolsRelational) = DroolsOr(this,rexpr)
  def || (bexpr:DroolsBoolean)    = DroolsOr(this,bexpr)
  def toMvelRelOp:String = ""
  def toMvel():String = {
    lhs.toMvel + toMvelRelOp + rhs.toMvel
  }
}

trait DroolsBoolean extends DroolsCondition {
  def lhs:DroolsCondition
  def rhs:DroolsCondition
  def && (rexpr:DroolsRelational) = DroolsAnd(this,rexpr)
  def && (bexpr:DroolsBoolean)    = DroolsAnd(this,bexpr)
  def || (rexpr:DroolsRelational) = DroolsOr(this,rexpr)
  def || (bexpr:DroolsBoolean)    = DroolsOr(this,bexpr)
  def toMvelBoolOp:String = ""
  def toMvel():String = {
   "(" + lhs.toMvel + toMvelBoolOp + rhs.toMvel + ")"
  }
}

case class DroolsEq(lhs:DroolsCondition, rhs:DroolsCondition)  extends DroolsRelational {
  override def toMvelRelOp:String = " == "
}


case class DroolsNeq(lhs:DroolsCondition, rhs:DroolsCondition)  extends DroolsRelational {
  override def toMvelRelOp:String = " != "
}

case class DroolsGte(lhs:DroolsCondition, rhs:DroolsCondition) extends DroolsRelational {
  override def toMvelRelOp:String = " >= "
}

case class DroolsGt(lhs:DroolsCondition, rhs:DroolsCondition)  extends DroolsRelational  {
  override def toMvelRelOp:String = " > "
}

case class DroolsLte(lhs:DroolsCondition, rhs:DroolsCondition) extends DroolsRelational  {
  override def toMvelRelOp:String = " <= "
}

case class DroolsLt(lhs:DroolsCondition, rhs:DroolsCondition)  extends DroolsRelational  {
  override def toMvelRelOp:String = " < "
}

case class DroolsAnd(lhs:DroolsCondition, rhs:DroolsCondition) extends DroolsBoolean {
  override def toMvelBoolOp:String = " && "
}

case class DroolsOr(lhs:DroolsCondition, rhs:DroolsCondition)  extends DroolsBoolean {
  override def toMvelBoolOp:String = " || "
}

case class DroolsExists(lhs:DroolsCondition)(implicit val namespace:LinkedHashMap[Symbol,DroolsCondition])  extends DroolsBoolean  {
  def rhs:DroolsCondition = DroolsNullCondition()
  namespace += ('exists -> this)
  override def toMvel():String = {
    " exists(" + lhs.toMvel + ")"
  }
}

case class DroolsNot(lhs:DroolsCondition)(implicit val namespace:LinkedHashMap[Symbol,DroolsCondition])  extends DroolsBoolean {
  def rhs:DroolsCondition = DroolsNullCondition()
  lhs match {
    case d:DroolsExists => namespace -= 'exists
    case _              =>
  }

  namespace += ('not -> this)
  override def toMvel():String = {
    " not(" + lhs.toMvel + ")"
  }
}

case class DroolsSequence(seq:Seq[DroolsCondition])             extends DroolsCondition  {
  def toMvel():String = ""
}
case class DroolsDecl(did:DroolsId,dexpr:DroolsCondition)       extends DroolsCondition  {
  def toMvel():String = {
    did.toMvel + " : " + dexpr.toMvel
  }
}

case class DroolsConstant(value:Any) extends DroolsCondition {

  def === (id:DroolsId)     = {
    val decl = DroolsEq(this,id)
    decl
  }

  def !== (id:DroolsId)     = {
    val decl = DroolsNeq(this,id)
    decl
  }

  def === (id:DroolsConstant)     = {
    val decl = DroolsEq(this,id)
    decl
  }

  def !== (id:DroolsConstant)     = {
    val decl = DroolsNeq(this,id)
    decl
  }

  def >  (id:DroolsId)    = {
    val decl = DroolsGt(this,id)
    decl
  }

  def >= (id:DroolsId)           = {
    val decl = DroolsGte(this,id)
    decl
  }

  def <  (id:DroolsId)           = {
    val decl = DroolsLt(this,id)
    decl
  }

  def <= (id:DroolsId)           = {
    val decl = DroolsLte(this,id)
    decl
  }

   def toMvel():String = {
     value match {
       case str:String
           => """"""" + str + """""""
       case x
          =>  x.toString
     }
   }
}

case class DroolsId(sym :Symbol)(implicit val namespace:LinkedHashMap[Symbol,DroolsCondition]) extends DroolsCondition {
  def * (id:DroolsId)     = {
    DroolsId( Symbol(sym.name +"."+id.sym.name))
  }

  def :=  (id:DroolsId)           = {
    val decl = DroolsDecl(this,id)
    namespace += (sym -> decl)
    decl
  }

  def :=  (tpl:DroolsTuple) = {
    val decl = DroolsDecl(this,tpl)
    namespace += (sym -> decl)
    decl
  }

  def === (id:DroolsId)     = {
    val decl = DroolsEq(this,id)
    decl
  }

  def !== (id:DroolsId)     = {
    val decl = DroolsNeq(this,id)
    decl
  }

  def === (id:DroolsConstant)     = {
    val decl = DroolsEq(this,id)
    decl
  }

  def !== (id:DroolsConstant)     = {
    val decl = DroolsNeq(this,id)
    decl
  }

  def >  (id:DroolsId)    = {
    val decl = DroolsGt(this,id)
    decl
  }

  def >  (id:DroolsConstant)    = {
    val decl = DroolsGt(this,id)
    decl
  }

  def >= (id:DroolsId)           = {
    val decl = DroolsGte(this,id)
    decl
  }

  def >= (id:DroolsConstant)           = {
    val decl = DroolsGte(this,id)
    decl
  }

  def <  (id:DroolsId)           = {
    val decl = DroolsLt(this,id)
    decl
  }

  def <  (id:DroolsConstant)           = {
    val decl = DroolsLt(this,id)
    decl
  }

  def <= (id:DroolsId)           = {
    val decl = DroolsLte(this,id)
    decl
  }

  def <= (id:DroolsConstant)           = {
    val decl = DroolsLte(this,id)
    decl
  }

  def toMvel():String = {
     sym match {
       case Symbol(s) => s
       case _         => ""
     }
  }

}

case class DroolsConstraint (expr:DroolsCondition*)(implicit val namespace:LinkedHashMap[Symbol,DroolsCondition]) extends DroolsCondition {
  expr.filter(p => p match {case d:DroolsDecl => true; case _ => false}).foreach(dd =>  dd match {case DroolsDecl(DroolsId(s),_) => namespace -= s ; case _=>})
  def toMvel():String = {
    val buf:StringBuffer = new StringBuffer
    expr.foreach(expr => buf.append(expr.toMvel() + ","))
    buf.deleteCharAt(buf.lastIndexOf(",")).toString
  }
}

case class DroolsTuple(t:(Symbol,DroolsCondition)) extends DroolsCondition {
  def toMvel():String = {
    (t._1 match {
       case Symbol(s) => s
       case _         => ""
     })  + "(" + t._2.toMvel() + ")"
  }
}

object DroolsConditionGenerator {
  implicit val namespace = LinkedHashMap.empty[Symbol,DroolsCondition]
  implicit def symbolToDroolsId(symbol:Symbol):DroolsId = DroolsId(symbol)
  implicit def tuple2ToDroolsTuple(tpl:(Symbol, DroolsCondition)):DroolsTuple = DroolsTuple(tpl)
  implicit def stringToDroolsConst(str:String):DroolsConstant = DroolsConstant(str)
  implicit def intToDroolsConst(int:Int):DroolsConstant = DroolsConstant(int)
  implicit def doubleToDroolsConst(double:Double):DroolsConstant = DroolsConstant(double)


  def when(exprs: => DroolsCondition){
    exprs
    namespace.foreach(t => println( t._2.toMvel + "\n"))
    namespace.clear
  }


  def ^     (exprs:DroolsCondition*):DroolsConstraint = DroolsConstraint(exprs:_*)
  def not   (expr:DroolsCondition):DroolsNot          = DroolsNot(expr)
  def exists(expr:DroolsCondition):DroolsExists       = DroolsExists(expr)
  def nothing:DroolsNullCondition                     = DroolsNullCondition()
}