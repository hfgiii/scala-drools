package org.hfgiii.drools.knowledgepackage

import org.drools.process.core.datatype.DataType
import org.jbpm.process.core.context.exception.ExceptionHandler
import org.jbpm.process.core.event.{EventFilter, EventTransformer}
import org.jbpm.ruleflow.core.factory._
import org.jbpm.ruleflow.core.{RuleFlowProcess, RuleFlowProcessFactory}

import scala.collection.JavaConversions._

class RuleflowProcessBuilder(factory:RuleFlowProcessFactory)   {

     def name(name : String) : RuleflowProcessBuilder   = {
          factory.name(name)

          this
     }

     def version(version : String) : RuleflowProcessBuilder   = {
          factory.version(version)

          this
     }

     def packageName(packageName : String) : RuleflowProcessBuilder   = {
          factory.packageName(packageName)

          this

     }

     def imports(imports : Array[String]) : RuleflowProcessBuilder   = {
          factory.imports(imports:_*)

          this
     }

     def functionImports(functionImports : Array[String]) : RuleflowProcessBuilder   = {
          factory.functionImports(functionImports:_*)

          this
     }

     def globals(globals : Map[String, String]) : RuleflowProcessBuilder   = {
          factory.globals(mapAsJavaMap(globals))

          this
     }

     def global(name : String, `type` : String) : RuleflowProcessBuilder   = {
         factory.global(name,`type`)

         this
     }

     def variable(name : String, `type` : DataType) : RuleflowProcessBuilder   = {
         factory.variable(name,`type`)

         this

     }

     def variable(name : String, `type` : DataType, value : AnyRef) : RuleflowProcessBuilder   = {
         factory.variable(name,`type`,value)

         this

     }

     def swimlane(name : String) : RuleflowProcessBuilder   = {
         factory.swimlane(name)

         this
     }

     def exceptionHandler(exception : String, exceptionHandler : ExceptionHandler) : RuleflowProcessBuilder   = {
         factory.exceptionHandler(exception,exceptionHandler)

         this
     }

     def exceptionHandler(exception : String, dialect : String, action : String) : RuleflowProcessBuilder   = {
         factory.exceptionHandler(exception,dialect,action)

         this
     }

     def validate : RuleflowProcessBuilder   = {
         factory.validate

         this
     }

     def startNode(id : Long) : StartNodeBuilder   = {
         new StartNodeBuilder(factory.startNode(id),this)
     }

     def endNode(id : Long) : EndNodeBuilder   = {
        new EndNodeBuilder(factory.endNode(id),this)
     }

     def actionNode(id : Long) : ActionNodeBuilder   = {
        new ActionNodeBuilder(factory.actionNode(id),this)
     }

     def milestoneNode(id : Long) : MilestoneNodeBuilder   = {
        new MilestoneNodeBuilder(factory.milestoneNode(id),this)
     }

     def timerNode(id : Long) : TimerNodeBuilder   = {
        new TimerNodeBuilder(factory.timerNode(id),this)
     }

     def humanTaskNode(id : Long) : HumanTaskNodeBuilder   = {
        new HumanTaskNodeBuilder(factory.humanTaskNode(id),this)
     }

     def subProcessNode(id : Long) : SubProcessNodeBuilder   = {
        new SubProcessNodeBuilder(factory.subProcessNode(id),this)
     }

     def splitNode(id : Long) : SplitNodeBuilder   = {
        new SplitNodeBuilder(factory.splitNode(id),this)
     }

     def joinNode(id : Long) : JoinNodeBuilder   = {
        new JoinNodeBuilder(factory.joinNode(id),this)
     }

     def ruleSetNode(id : Long) : RulesetNodeBuilder   = {
        new RulesetNodeBuilder(factory.ruleSetNode(id),this)
     }

     def faultNode(id : Long) : FaultNodeBuilder   = {
         new FaultNodeBuilder(factory.faultNode(id),this)
     }

     def eventNode(id : Long) : EventNodeBuilder   = {
         new EventNodeBuilder(factory.eventNode(id),this)
     }

     def compositeNode(id : Long) : CompositeNodeBuilder   = {
         new CompositeNodeBuilder(factory.compositeNode(id),this)
     }

     def forEachNode(id : Long) : ForEachNodeBuilder   = {
         new ForEachNodeBuilder(factory.forEachNode(id),this)
     }

     def workItemNode(id : Long) : WorkItemNodeBuilder   = {
         new WorkItemNodeBuilder(factory.workItemNode(id),this)
     }

     def connection(fromId : Long, toId : Long) : RuleflowProcessBuilder   = {
          factory.connection(fromId,toId)

          this
     }

     def done       : RuleflowProcessBuilder   = {
         factory done

         this
     }

     def getProcess : RuleFlowProcess    = {
         factory getProcess
     }

}

object RuleflowProcessBuilder {
   def apply(processName:String):RuleflowProcessBuilder = new RuleflowProcessBuilder(RuleFlowProcessFactory.createProcess(processName))

   def newProcess(processName:String)(implicit process: (RuleflowProcessBuilder) => RuleFlowProcess):RuleFlowProcess = process(apply(processName))
}


class StartNodeBuilder(factory:StartNodeFactory,parent:RuleflowProcessBuilder)  {

    def name(name: String): StartNodeBuilder = {
           factory.name(name)

           this
    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}

class EndNodeBuilder(factory:EndNodeFactory,parent:RuleflowProcessBuilder)   {

    def name(name: String): EndNodeBuilder  = {
      factory.name(name)

      this
    }

    def terminate(terminate: Boolean): EndNodeBuilder  = {
      factory.terminate(terminate)

      this
    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}

class ActionNodeBuilder(factory:ActionNodeFactory,parent:RuleflowProcessBuilder)   {

    def name(name: String): ActionNodeBuilder = {
         factory.name(name)

         this
    }

    def action(dialect: String, action: String): ActionNodeBuilder = {
         factory.action(dialect,action)

          this
    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}

class MilestoneNodeBuilder(factory:MilestoneNodeFactory,parent:RuleflowProcessBuilder)     {


    def name(name: String): MilestoneNodeBuilder  = {
         factory.name(name)

         this
    }

    def onEntryAction(dialect: String, action: String): MilestoneNodeBuilder = {
         factory.onEntryAction(dialect,action)

         this
    }

    def onExitAction(dialect: String, action: String): MilestoneNodeBuilder = {
         factory.onExitAction(dialect,action)

         this
    }

    def constraint(constraint: String): MilestoneNodeBuilder = {
         factory.constraint(constraint)

         this
    }

    def timer(delay: String, period: String, dialect: String, action: String): MilestoneNodeBuilder = {
         factory.timer(delay,period,dialect,action)

         this
    }


    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }


}

class TimerNodeBuilder(factory:TimerNodeFactory,parent:RuleflowProcessBuilder)     {
    def name(name: String): TimerNodeBuilder  = {
         factory.name(name)

         this
    }

    def delay(delay: String): TimerNodeBuilder = {
         factory.delay(delay)

         this
    }

    def period(period: String): TimerNodeBuilder = {
         factory.period(period)

         this
    }


    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}

class HumanTaskNodeBuilder(factory:HumanTaskNodeFactory,parent:RuleflowProcessBuilder) {

    def name(name: String): HumanTaskNodeBuilder  = {
         factory.name(name)

         this
    }

    def taskName(taskName: String): HumanTaskNodeBuilder   = {
         factory.taskName(taskName)

         this
    }

    def actorId(actorId: String): HumanTaskNodeBuilder    = {
        factory.actorId(actorId)

        this
    }

    def priority(priority: String): HumanTaskNodeBuilder    = {
        factory.priority(priority)

        this
    }

    def comment(comment: String): HumanTaskNodeBuilder        = {
        factory.comment(comment)

        this
    }

    def skippable(skippable: Boolean): HumanTaskNodeBuilder    = {
        factory.skippable(skippable)

        this
    }

    def content(content: String): HumanTaskNodeBuilder     = {
        factory.content(content)

        this
    }

    def inMapping(parameterName: String, variableName: String): HumanTaskNodeBuilder    = {
        factory.inMapping(parameterName,variableName)

        this
    }

    def outMapping(parameterName: String, variableName: String): HumanTaskNodeBuilder     = {
        factory.outMapping(parameterName,variableName)

        this
    }

    def waitForCompletion(waitForCompletion: Boolean): HumanTaskNodeBuilder       = {
        factory.waitForCompletion(waitForCompletion)

        this
    }

    def swimlane(swimlane: String): HumanTaskNodeBuilder           = {
        factory.swimlane(swimlane)

        this
    }

    def onEntryAction(dialect: String, action: String): HumanTaskNodeBuilder   = {
        factory.onEntryAction(dialect,action)

        this
    }

    def onExitAction(dialect: String, action: String): HumanTaskNodeBuilder  = {
        factory.onExitAction(dialect,action)

        this

    }

    def timer(delay: String, period: String, dialect: String, action: String): HumanTaskNodeBuilder   = {
        factory.timer(delay,period,dialect,action)

        this
    }

    def done: RuleflowProcessBuilder = {
        parent done

        parent
    }
}

class SubProcessNodeBuilder(factory:SubProcessNodeFactory,parent:RuleflowProcessBuilder)  {
    def name(name: String): SubProcessNodeBuilder   = {

         factory.name(name)

         this
    }

    def processId(processId: String): SubProcessNodeBuilder  = {
         factory.processId(processId)

         this
    }

    def waitForCompletion(waitForCompletion: Boolean): SubProcessNodeBuilder = {
         factory.waitForCompletion(waitForCompletion)

         this
    }

    def inMapping(parameterName: String, variableName: String): SubProcessNodeBuilder = {
        factory.inMapping(parameterName, variableName)

        this
    }

    def outMapping(parameterName: String, variableName: String): SubProcessNodeBuilder = {
        factory.outMapping(parameterName,variableName)

        this
    }

    def independent(independent: Boolean): SubProcessNodeBuilder = {
        factory.independent(independent)

        this
    }

    def onEntryAction(dialect: String, action: String): SubProcessNodeBuilder  = {
        factory.onEntryAction(dialect,action)

        this
    }

    def onExitAction(dialect: String, action: String): SubProcessNodeBuilder  = {
        factory.onExitAction(dialect,action)

        this

    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }

}


class SplitNodeBuilder(factory:SplitFactory,parent:RuleflowProcessBuilder)   {

    def name(name: String): SplitNodeBuilder   = {
         factory.name(name)

         this
    }

    def `type`(`type` : Int): SplitNodeBuilder   = {
         factory.`type`(`type`)

         this
    }

    def constraint(toNodeId: Long, name: String, `type` : String, dialect: String, constraint: String): SplitNodeBuilder  = {
        factory.constraint(toNodeId,name,`type`,dialect,constraint)

        this
    }

    def constraint(toNodeId: Long, name: String, `type` : String, dialect: String, constraint: String, priority: Int): SplitNodeBuilder  = {
       factory.constraint(toNodeId,name,`type`,dialect,constraint,priority)

       this
    }

    def done: RuleflowProcessBuilder = {

        factory done

        parent
    }
}

class JoinNodeBuilder(factory:JoinFactory,parent:RuleflowProcessBuilder)   {
    def name(name: String): JoinNodeBuilder    = {
       factory.name(name)

       this
    }

    def `type`(`type` : Int): JoinNodeBuilder   = {
        factory.`type`(`type`)

        this
    }

    def `type`(n: String): JoinNodeBuilder      = {
        factory.`type`(n)

        this

    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }

}

class RulesetNodeBuilder(factory:RuleSetNodeFactory,parent:RuleflowProcessBuilder)  {
    def name(name: String): RulesetNodeBuilder = {
       factory.name(name)

       this
    }

    def ruleFlowGroup(ruleFlowGroup: String): RulesetNodeBuilder = {
       factory.ruleFlowGroup(ruleFlowGroup)

       this
    }

    def timer(delay: String, period: String, dialect: String, action: String): RulesetNodeBuilder = {
       factory.timer(delay,period,dialect,action)

       this
    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}

class FaultNodeBuilder(factory:FaultNodeFactory,parent:RuleflowProcessBuilder)   {
    def name(name: String): FaultNodeBuilder = {
       factory.name(name)

       this
    }

    def setFaultVariable(faultVariable: String): FaultNodeBuilder = {
      factory.setFaultVariable(faultVariable)

      this
    }

    def setFaultName(faultName: String): FaultNodeBuilder  = {
      factory.setFaultName(faultName)

      this
    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}

class EventNodeBuilder(factory:EventNodeFactory,parent:RuleflowProcessBuilder)   {

    def name(name: String): EventNodeBuilder    = {
      factory.name(name)

      this
    }

    def variableName(variableName: String): EventNodeBuilder  = {
       factory.variableName(variableName)

       this
    }

    def eventFilter(eventFilter: EventFilter): EventNodeBuilder = {
       factory.eventFilter(eventFilter)

       this
    }

    def eventType(eventType: String): EventNodeBuilder = {
        factory.eventType(eventType)

        this
    }

    def eventTransformer(transformer: EventTransformer): EventNodeBuilder = {
         factory.eventTransformer(transformer)

         this

    }

    def scope(scope: String): EventNodeBuilder = {
         factory.scope(scope)

         this
    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}

class CompositeNodeBuilder(factory:CompositeNodeFactory,parent:RuleflowProcessBuilder)   {

    def variable(name: String, `type` : DataType): CompositeNodeBuilder = {
       factory.variable(name,`type`)

       this
    }

    def variable(name: String, `type` : DataType, value: AnyRef): CompositeNodeBuilder = {
       factory.variable(name,`type`,value)

       this

    }

    def exceptionHandler(exception: String, exceptionHandler: ExceptionHandler): CompositeNodeBuilder = {
        factory.exceptionHandler(exception,exceptionHandler)

        this
    }

    def exceptionHandler(exception: String, dialect: String, action: String): CompositeNodeBuilder = {
        factory.exceptionHandler(exception,dialect,action)

        this

    }

    def linkIncomingConnections(nodeId: Long): CompositeNodeBuilder  = {
        factory.linkIncomingConnections(nodeId)


        this
    }

    def linkOutgoingConnections(nodeId: Long): CompositeNodeBuilder  = {
        factory.linkOutgoingConnections(nodeId)


        this

    }

    def done: RuleflowProcessBuilder  = {
        parent done

        parent
    }
}

class ForEachNodeBuilder(factory:ForEachNodeFactory,parent:RuleflowProcessBuilder)      {
    def collectionExpression(collectionExpression: String): ForEachNodeBuilder   = {
       factory.collectionExpression(collectionExpression)

       this
    }

    def variable(variableName: String, dataType: DataType): ForEachNodeBuilder = {
       factory.variable(variableName,dataType)

        this
    }

    def waitForCompletion(waitForCompletion: Boolean): ForEachNodeBuilder  = {
        factory.waitForCompletion(waitForCompletion)

        this
    }

    def linkIncomingConnections(nodeId: Long): ForEachNodeBuilder  = {
        factory.linkIncomingConnections(nodeId)

        this
    }

    def linkOutgoingConnections(nodeId: Long): ForEachNodeBuilder = {
        factory.linkOutgoingConnections(nodeId)

        this
    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}

class WorkItemNodeBuilder(factory:WorkItemNodeFactory,parent:RuleflowProcessBuilder)  {

    def name(name: String): WorkItemNodeBuilder  = {
        factory.name(name)

        this
    }

    def waitForCompletion(waitForCompletion: Boolean): WorkItemNodeBuilder  = {
        factory.waitForCompletion(waitForCompletion)

        this
    }

    def inMapping(parameterName: String, variableName: String): WorkItemNodeBuilder   = {
        factory.inMapping(parameterName,variableName)

        this
    }

    def outMapping(parameterName: String, variableName: String): WorkItemNodeBuilder = {
        factory.outMapping(parameterName,variableName)

        this
    }

    def workName(name: String): WorkItemNodeBuilder         = {
        factory.workName(name)

        this
    }

    def workParameter(name: String, value: AnyRef): WorkItemNodeBuilder   = {
        factory.workParameter(name,value)

        this
    }

    def workParameterDefinition(name: String, dataType: DataType): WorkItemNodeBuilder = {
        factory.workParameterDefinition(name,dataType)

        this
    }

    def onEntryAction(dialect: String, action: String): WorkItemNodeBuilder  = {
        factory.onEntryAction(dialect,action)

        this
    }

    def onExitAction(dialect: String, action: String): WorkItemNodeBuilder   = {
        factory.onEntryAction(dialect,action)

        this
    }

    def timer(delay: String, period: String, dialect: String, action: String): WorkItemNodeBuilder  = {
        factory.timer(delay,period,dialect,action)

        this
    }

    def done: RuleflowProcessBuilder = {
        factory done

        parent
    }
}