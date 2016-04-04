package org.hfgiii.drools.knowledgepackage

import org.drools.compiler.{PackageBuilder, PackageRegistry}
import org.drools.rule.builder.ConsequenceBuilder
import org.drools.rule.builder.dialect.mvel.MVELDialect


class ScalaEmbeddedDialect(builder: PackageBuilder,
                           pkgRegistry: PackageRegistry,
                           pkg: org.drools.rule.Package)
      extends MVELDialect (builder, pkgRegistry, pkg, "embedded-scala") {

  override def getConsequenceBuilder: ConsequenceBuilder = ScalaConsequenceBuilder

}