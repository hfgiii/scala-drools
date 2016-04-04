package org.hfgiii.drools

import org.drools.builder.DecisionTableInputType._

class TestDecisionTablesWithCSV extends TestDecisionTables {

   addDecisionTablePaths(("interest calculation.csv",CSV))


}