package org.hfgiii.drools.knowledgepackage

import org.drools.compiler.{Dialect, PackageBuilder, PackageRegistry}
import org.drools.rule.builder.dialect.mvel.MVELDialectConfiguration

class ScalaEmbeddedDialectConfiguration extends MVELDialectConfiguration {

  override def newDialect(packageBuilder: PackageBuilder, pkgRegistry: PackageRegistry, pkg: org.drools.rule.Package): Dialect = {
    return new ScalaEmbeddedDialect(packageBuilder, pkgRegistry, pkg)
  }
}