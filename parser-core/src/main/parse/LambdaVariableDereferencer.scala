// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import org.nlogo.core.{prim, AstTransformer, ReporterApp, Fail, I18N},
  prim.{ _constcodeblock, _createreporter, _lambdavariable, _reportertask },
  Fail._

import scala.collection.mutable

// This AstTransformer collects the variables of a given lambda and
// attaches them to that lambda.
class LambdaVariableDereferencer extends AstTransformer {
  override def visitReporterApp(app: ReporterApp): ReporterApp = {
    app.reporter match {
      case cr: _createreporter =>
        val names = app.args(0) match {
          // we already know this is a code block
          case a: ReporterApp => a.reporter match {
            case c: _constcodeblock =>
              c.value.map(_.text.toUpperCase)
            case _ => Seq()
          }
          case o => Seq()
        }
        val body = super.visitExpression(app.args(1) match {
          case a: ReporterApp if a.reporter.isInstanceOf[_reportertask] => a.args(0)
          case _ => app.args(1)
        })
        val newRep = cr.copy(argumentNames = names)
        app.copy(reporter = newRep, args = app.args.updated(1, body))
      case _ => super.visitReporterApp(app)
    }
  }
}
