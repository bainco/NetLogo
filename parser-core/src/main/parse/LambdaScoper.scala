// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import org.nlogo.core.{ Fail, Token, TokenType }, Fail.exception

import org.nlogo.core.prim.{ _createreporter, _letvariable, _lambdavariable, _unknownidentifier }

object LambdaScoper {
  sealed trait State
  case object NoLambda extends State
  case object EnterArgs extends State
  case class Arguments(args: Seq[String]) extends State
  case class EnterBody(args: Seq[String]) extends State
  case class Body(args: Seq[String]) extends State
}

class LambdaScoper(usedNames: Map[String, SymbolType])
  extends TokenTransformer[Seq[LambdaScoper.State]] {

  import LambdaScoper._
  def initialState = Seq(NoLambda)

  def transform(token: Token, states: Seq[State]): (Token, Seq[State]) = {
    val state = states.head
    def update(t: Token, s: State) = (t, states.updated(0, s))
    def push(t: Token, s: State)   = (t, s +: states)
    def pop(t: Token)              = (t, states.tail)
    def nestingLambdasContain(varName: String) = states.tail.exists {
      case b: Body => b.args.contains(varName)
      case _ => false
    }
    (token, states.head) match {
      case (t, _: Arguments) if usedNames.isDefinedAt(t.text.toUpperCase) =>
        SymbolType.alreadyDefinedException(usedNames(t.text.toUpperCase), t)
      case (t, a: Arguments) if a.args.contains(t.text.toUpperCase) =>
        SymbolType.alreadyDefinedException(SymbolType.LocalVariable, t)
      case (t, a: Arguments) if nestingLambdasContain(t.text.toUpperCase) =>
        SymbolType.alreadyDefinedException(SymbolType.LocalVariable, t)
        // this condition assumes that LetScoper has processed the token stream before LambdaScoper
      case (t @ Token(tokenText, TokenType.Reporter, _letvariable(_)), b: Arguments) =>
        SymbolType.alreadyDefinedException(SymbolType.LocalVariable, t)
      case (Token(_, TokenType.Literal, _), _: Arguments)      =>
        exception("Expected a variable name here", token)
      case (Token(_, _, lambda: _createreporter), b: Body)     => push(token, EnterArgs)
      case (Token(_, _, lambda: _createreporter), _)           => update(token, EnterArgs)
      case (Token(_, TokenType.OpenBracket, _),     EnterArgs) => update(token, Arguments(Seq()))
      case (Token(_, TokenType.OpenBracket, _),  e: EnterBody) => update(token, Body(e.args))
      case (Token(_, TokenType.CloseBracket, _), a: Arguments) => update(token, EnterBody(a.args))
      case (t @ Token(tokenText, TokenType.Reporter, _unknownidentifier()), b: Body)
      if b.args.contains(tokenText.toUpperCase) =>
        update(t.refine(_lambdavariable(t.text.toUpperCase)), NoLambda)
      case (t @ Token(_, TokenType.CloseBracket, _), b: Body) =>
        if (states.length == 1) update(token, NoLambda) else pop(t)
      case (t, EnterArgs)    => if (states.length == 1) update(t, NoLambda) else pop(t)
      case (t, _: EnterBody) => if (states.length == 1) update(t, NoLambda) else pop(t)
      case (t, a: Arguments) => update(t, Arguments(a.args :+ t.text.toUpperCase))
      case _                 => update(token, state)
    }
  }
}
