// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import org.scalatest.FunSuite

import org.nlogo.core.{ CompilerException, Token, TokenType }
import org.nlogo.core.prim.{ _ask, _createreporter }
import org.nlogo.core.TokenDSL._

class LambdaScoperTests extends FunSuite {
  import LambdaScoper._

  val usedNames = Map(
    "ASK"  -> SymbolType.PrimitiveCommand,
    "MEAN" -> SymbolType.PrimitiveReporter,
    "FOO"  -> SymbolType.GlobalVariable)

  val lambdaScoper = new LambdaScoper(usedNames)

  val lambda = Token("create-reporter", TokenType.Reporter, _createreporter())(0, 0, "test")

  def testStateTransition(token: Token, initialState: State, expectedState: State): Unit = {
    test(s"when in state: ${initialState} encounters token: ${token.text}, LambdaScoper emits ${token.text} and enters state ${expectedState}") {
      assertResult((token, expectedState))(lambdaScoper.transform(token, initialState))
    }
  }

  def testErrorCondition(t: Token, state: State, error: String): Unit = {
    test(s"""when in state ${state}, lambdaScoper errors on ${t} with: "${error}"""") {
      val ex = intercept[CompilerException] { lambdaScoper.transform(t, state) }
      assert(ex.getMessage.contains(error))
    }
  }

  def arguments(varNames: String*): Arguments = Arguments(varNames.map(_.toUpperCase))

  testStateTransition(lit(2), NoLambda, NoLambda)
  testStateTransition(lambda, NoLambda, EnterArgs)
  testStateTransition(`[`, EnterArgs, arguments())
  // this error condition is detected by the parser, so we just pretend we didn't see anything
  testStateTransition(lit(3), EnterArgs, NoLambda)
  testStateTransition(`]`, arguments(), EnterBody(Seq()))
  testStateTransition(id("bar"), arguments(), arguments("bar"))
  testStateTransition(id("baz"), arguments("bar"), arguments("bar", "baz"))
  testStateTransition(`]`, arguments("bar"), EnterBody(Seq("BAR")))
  // this is a case where if we don't see what we're expecting we count on ExpressionParser to error
  testStateTransition(lit(3), EnterBody(Seq("BAR")), NoLambda)
  testStateTransition(`[`, EnterBody(Seq("BAR")), Body(Seq("BAR")))
  testStateTransition(`]`, Body(Seq("BAR")), NoLambda)
  test("transforms body tokens matching arguments from unknown identifiers to lambda vars") {
    val (t, _) = lambdaScoper.transform(unid("foo"), Body(Seq("FOO")))
    assert(t.value == org.nlogo.core.prim._lambdavariable("FOO"))
  }
  test("does not transform body tokens not matching lambda arguments") {
    val (t, _) = lambdaScoper.transform(unid("foo"), Body(Seq("BAR")))
    assert(t == unid("foo"))
  }

  // TODO: test nesting....

  testErrorCondition(Token("mean", TokenType.Reporter, null)(0, 0, "test"), arguments(), "There is already a primitive reporter called MEAN")
  testErrorCondition(Token("ask", TokenType.Command, null)(0, 0, "test"), arguments(), "There is already a primitive command called ASK")
  testErrorCondition(Token("foo", TokenType.Ident, null)(0, 0, "test"), arguments(), "There is already a global variable called FOO")
  testErrorCondition(Token("bar", TokenType.Reporter, null)(0, 0, "test"), arguments("bar"), "There is already a local variable here called BAR")
  testErrorCondition(lit(5), arguments(), "Expected a variable name here")
}
