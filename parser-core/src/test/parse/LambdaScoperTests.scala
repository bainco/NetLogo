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

  def transform(token: Token, state: State): (Token, State) = {
    val (t, ss) = transformAll(token, Seq(state))
    (t, ss.head)
  }

  def transformAll(token: Token, states: Seq[State]): (Token, Seq[State]) = {
    lambdaScoper.transform(token, states)
  }

  def testStateTransition(token: Token, initialState: State, expectedState: State): Unit = {
    test(s"when in state: ${initialState} encounters token: ${token.text}, LambdaScoper emits ${token.text} and enters state ${expectedState}") {
      assertResult((token, expectedState))(transform(token, initialState))
    }
  }

  def testErrorCondition(t: Token, state: State, error: String): Unit = {
    test(s"""when in state ${state}, lambdaScoper errors on ${t} with: "${error}"""") {
      val ex = intercept[CompilerException] { transform(t, state) }
      assert(ex.getMessage.contains(error))
    }
  }

  def testTokenTransformation(description: String, tok: Token, state: State, assertion: Token => Boolean) = {
    test(description) {
      val (t, _) = transform(tok, state)
      assert(assertion(t))
    }
  }

  def arguments(varNames: String*): Arguments = Arguments(varNames.map(_.toUpperCase))

  testStateTransition(lit(2), NoLambda, NoLambda)
  testStateTransition(lambda, NoLambda, EnterArgs)
  testStateTransition(`[`, EnterArgs, arguments())
  testStateTransition(`]`, arguments(), EnterBody(Seq()))
  testStateTransition(id("bar"), arguments(), arguments("bar"))
  testStateTransition(id("baz"), arguments("bar"), arguments("bar", "baz"))
  testStateTransition(`]`, arguments("bar"), EnterBody(Seq("BAR")))
  testStateTransition(`[`, EnterBody(Seq("BAR")), Body(Seq("BAR")))
  testStateTransition(`]`, Body(Seq("BAR")), NoLambda)
  testTokenTransformation(
    "transforms body tokens matching arguments from unknown identifiers to lambda vars",
    unid("foo"), Body(Seq("FOO")), _.value == org.nlogo.core.prim._lambdavariable("FOO"))
  testTokenTransformation(
    "does not transform body tokens not matching lambda arguments",
    unid("foo"), Body(Seq("BAR")), _ == unid("foo"))

  test("test nested lambda push") {
    val (t, s) = transformAll(lambda, Seq(Body(Seq())))
    assert(t == lambda)
    assert(s.length == 2)
    assert(s.head == EnterArgs)
    assert(s(1) == Body(Seq()))
  }

  test("test nested lambda pop") {
    val (t, s) = transformAll(`]`, Seq(Body(Seq()), Body(Seq())))
    assert(s.length == 1)
    assert(s.head == Body(Seq()))
  }

  test("test nested lambda errors with duplicate variable names") {
    val ex = intercept[CompilerException] { transformAll(unid("BAR"), Seq(Arguments(Seq()), Body(Seq("BAR")))) }
    assert(ex.getMessage.contains("There is already a local variable here called BAR"))
  }

  testErrorCondition(Token("mean", TokenType.Reporter, null)(0, 0, "test"), arguments(), "There is already a primitive reporter called MEAN")
  testErrorCondition(Token("ask", TokenType.Command, null)(0, 0, "test"), arguments(), "There is already a primitive command called ASK")
  testErrorCondition(Token("foo", TokenType.Ident, null)(0, 0, "test"), arguments(), "There is already a global variable called FOO")
  testErrorCondition(Token("bar", TokenType.Reporter, null)(0, 0, "test"), arguments("bar"), "There is already a local variable here called BAR")
  testErrorCondition(lit(5), arguments(), "Expected a variable name here")
  testErrorCondition(letvar("baz"), arguments(), "There is already a local variable here called BAZ")

  // the following error conditions are detected by the parser, so just pretend we didn't see anything
  testStateTransition(lit(3), EnterArgs, NoLambda)
  testStateTransition(lit(3), EnterBody(Seq("BAR")), NoLambda)

  test("test nested lambda error") {
    val (t, s) = transformAll(lit(3), Seq(EnterArgs, Body(Seq())))
    assert(s.length == 1)
    assert(s.head == Body(Seq()))
  }

  test("test nested lambda error entering body") {
    val (t, s) = transformAll(lit(3), Seq(EnterBody(Seq()), Body(Seq())))
    assert(s.length == 1)
    assert(s.head == Body(Seq()))
  }
}
