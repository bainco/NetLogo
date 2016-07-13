// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import org.scalatest.FunSuite

import org.nlogo.core.CompilerException
import org.nlogo.core.TokenDSL._

/*
class LambdaScoperTests extends FunSuite {
  test("introduces no scope when there are no variables in the lambda variable block") {
    assert(LambdaScoper(Seq(`[`, `]`)) == Seq())
  }
  test("errors when a variable in the lambda variable block isn't a variable") {
    intercept[CompilerException] {
      LambdaScoper(Seq(`[`, lit(2), `]`))
    }
  }
  test("introduces scope for each variable in the lambda variable block") {
    pending
  }
}
*/
