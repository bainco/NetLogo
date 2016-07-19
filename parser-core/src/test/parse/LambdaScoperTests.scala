// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import org.scalatest.FunSuite

import org.nlogo.core.{ CompilerException, Token, TokenType }
import org.nlogo.core.prim.{ _ask, _createreporter, _lambdavariable }
import org.nlogo.core.TokenDSL._

class LambdaScoperTests extends FunSuite {
  import LambdaScoper._

  val usedNames = Map(
    "ASK"  -> SymbolType.PrimitiveCommand,
    "MEAN" -> SymbolType.PrimitiveReporter,
    "FOO"  -> SymbolType.GlobalVariable)

  val lambdaScoper = new LambdaScoper(usedNames)

  val lambda = Token("create-reporter", TokenType.Reporter, new _createreporter())(0, 0, "test")

  def transform(token: Token, state: State): (Token, State) = {
    val (t, ss) = transformAll(token, Seq(state))
    (t, ss.head)
  }

  def transformAll(token: Token, states: Seq[State]): (Token, Seq[State]) = {
    lambdaScoper.transform(token, states)
  }

  def testErrorCondition(t: Token, state: State, error: String): Unit = {
    test(s"""when in state ${state}, lambdaScoper errors on ${t} with: "${error}"""") {
      val ex = intercept[CompilerException] { transform(t, state) }
      assert(ex.getMessage.contains(error))
    }
  }

  def nameState(state: State): String = {
    def descriptor(args: Seq[String]) = if (args.isEmpty) "(no arguments)" else args.mkString("[ ", " ", " ]")
    state match {
      case NoLambda     => "NoLambda"
      case EnterArgs    => "EnterArgs"
      case a: Arguments => s"Arguments ${descriptor(a.args)}"
      case e: EnterBody => s"EnterBody ${descriptor(e.args)}"
      case b: Body      => s"Body ${descriptor(b.args)}"
    }
  }

  def testLambda(token: Token, state: State,
    newTokenIs: (String, (Token, Token) => Boolean),
    newStateIs: (String, (State, State) => Boolean)): Unit = {
    test(s"When starting in state ${nameState(state)}, and given ${token.text}, emits ${newTokenIs._1} and ${newStateIs._1}") {
      val (newToken, newState) = transform(token, state)
      assert(newTokenIs._2(token, newToken), s"expected ${newTokenIs._1}, got $newToken")
      assert(newStateIs._2(state, newState), s"expected ${newStateIs._1}, got $newState")
    }
  }

  def arguments(varNames: String*): Arguments = Arguments(varNames.map(_.toUpperCase))

  val sameToken = ("the same token", (before: Token, after: Token) => before == after)
  val sameState = ("stays in the same state", (before: State, after: State) => before == after)
  val changedToSymbol = ("the token as a symbol",
    (before: Token, after: Token) => after.value == org.nlogo.core.prim._symbol() && after.text == before.text)
  def transition(state: State) = (s"transitions to ${nameState(state)}", (_: State, after: State) => after == state)
  val lambdaVariable = (s"the token as a lambda variable",
    (before: Token, after: Token) => before.text == after.text && after.value == _lambdavariable(before.text.toUpperCase))
  testLambda(lit(2),      NoLambda,         newTokenIs = sameToken,         newStateIs = sameState)
  testLambda(lambda,      NoLambda,         newTokenIs = sameToken,         newStateIs = transition(EnterArgs))
  testLambda(`[`,         EnterArgs,        newTokenIs = sameToken,         newStateIs = transition(arguments()))
  testLambda(`]`,         arguments(),      newTokenIs = sameToken,         newStateIs = transition(EnterBody(Seq())))
  testLambda(id("bar"),   arguments(),      newTokenIs = changedToSymbol,   newStateIs = transition(arguments("bar")))
  testLambda(id("baz"),   arguments("bar"), newTokenIs = changedToSymbol,   newStateIs = transition(arguments("bar", "baz")))
  testLambda(`]`,         arguments("bar"), newTokenIs = sameToken,         newStateIs = transition(EnterBody(Seq("BAR"))))
  testLambda(`[`,         EnterBody(Seq("BAR")), newTokenIs = sameToken,    newStateIs = transition(Body(Seq("BAR"))))
  testLambda(`]`,         Body(Seq("BAR")), newTokenIs = sameToken,         newStateIs = transition(NoLambda))
  testLambda(unid("foo"), Body(Seq("FOO")), newTokenIs = lambdaVariable,    newStateIs = sameState)
  testLambda(unid("foo"), Body(Seq("BAR")), newTokenIs = sameToken,         newStateIs = sameState)
  testLambda(`;`("comment"), EnterBody(Seq("BAR")), newTokenIs = sameToken, newStateIs = sameState)
  // the following error conditions are detected by the parser, so just pretend we didn't see anything
  testLambda(lit(3),      EnterArgs,        newTokenIs = sameToken,         newStateIs = transition(NoLambda))
  testLambda(lit(3),      EnterBody(Seq("BAR")), newTokenIs = sameToken,    newStateIs = transition(NoLambda))

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
