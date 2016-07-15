// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import org.nlogo.core.{ Fail, Token, TokenType }, Fail.exception

import org.nlogo.core.prim.{ _createreporter, _lambdavariable, _unknownidentifier }

object LambdaScoper {
  sealed trait State
  case object NoLambda extends State
  case object EnterArgs extends State
  case class Arguments(args: Seq[String]) extends State
  case class EnterBody(args: Seq[String]) extends State
  case class Body(args: Seq[String]) extends State
}

class LambdaScoper(usedNames: Map[String, SymbolType])
  extends TokenTransformer[LambdaScoper.State] {

  import LambdaScoper._
  def initialState = NoLambda
  def transform(token: Token, state: State): (Token, State) =
    (token, state) match {
      case (t, _: Arguments) if usedNames.isDefinedAt(t.text.toUpperCase) =>
        SymbolType.alreadyDefinedException(usedNames(t.text.toUpperCase), t)
      case (t, a: Arguments) if a.args.contains(t.text.toUpperCase) =>
        SymbolType.alreadyDefinedException(SymbolType.LocalVariable, t)
      case (Token(_, TokenType.Literal, _), _: Arguments) =>
        exception("Expected a variable name here", token)
      case (Token(_, _, lambda: _createreporter), _) => (token, EnterArgs)
      case (Token(_, TokenType.OpenBracket, _),     EnterArgs) => (token, Arguments(Seq()))
      case (Token(_, TokenType.OpenBracket, _),  e: EnterBody) => (token, Body(e.args))
      case (Token(_, TokenType.CloseBracket, _), a: Arguments) => (token, EnterBody(a.args))
      case (Token(_, TokenType.CloseBracket, _), b: Body)      => (token, NoLambda)
      case (t@Token(tokenText, TokenType.Reporter, _unknownidentifier()), b: Body) if b.args.contains(tokenText.toUpperCase) =>
        (t.refine(_lambdavariable(t.text.toUpperCase)), NoLambda)
      case (t, EnterArgs)    => (t, NoLambda)
      case (_, _: EnterBody) => (token, NoLambda)
      case (t, a: Arguments) => (t, Arguments(a.args :+ t.text.toUpperCase))
      case _ => (token, state)
    }
}
