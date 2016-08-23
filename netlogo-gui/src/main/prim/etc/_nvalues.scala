// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc

import org.nlogo.core.{ I18N, LogoList }
import org.nlogo.api.{ LogoListBuilder}
import org.nlogo.core.Syntax
import org.nlogo.nvm.{ AnonymousProcedure, Context, EngineException, Reporter }

class _nvalues extends Reporter {

  override def report(context: Context) = {
    // get the first argument...
    val n = argEvalIntValue(context, 0)
    if (n < 0)
      throw new EngineException( context, this,
        I18N.errors.getN("org.nlogo.prim.etc.$common.noNegativeNumber", displayName))
    // make the result list.
    val result = new LogoListBuilder
    val task = argEvalReporter(context, 1)
    if (task.syntax.minimum > 1)
      throw new EngineException(context, this, AnonymousProcedure.missingInputs(task, 1))
    for (i <- 0 until n)
      result.add(task.report(context, Array[AnyRef](Double.box(i))))
    result.toLogoList
  }

}
