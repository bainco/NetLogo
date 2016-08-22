// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import org.nlogo.core.{ AstNode, CommandBlock, Dump, Instruction, LogoList, ProcedureDefinition,
  ReporterApp, ReporterBlock, Statement, prim },
  prim.{ _commandtask, _commandlambda, _const, _lambdavariable, _reporterlambda, _reportertask, _taskvariable }

import WhiteSpace._

object Formatter {

  type Operation = (Formatter, AstNode, AstPath, Context) => Context
  case class Context(
    text: String,
    operations: Map[AstPath, Operation],
    instructionToString: Instruction => String = instructionString _,
    wsMap: WhitespaceMap = WhitespaceMap.empty) {
      def appendText(t: String): Context = copy(text = text + t)
    }

  def instructionString(i: Instruction): String =
    i match {
      case _const(value) if value.isInstanceOf[LogoList] =>
        println(i.token + ": "+ value)
        Dump.logoObject(value, true, false)
      case r: _const        => r.token.text
      case r: _reportertask => ""
      case r: _commandtask  => ""
      case r: _commandlambda => ""
      case v: _taskvariable   if v.synthetic => ""
      case v: _lambdavariable if v.synthetic => ""
      case r                => r.token.text
    }

  def deletedInstructionToString(i: Instruction): String = ""
}

class Formatter
  extends PositionalAstFolder[Formatter.Context] {

  import Formatter.{ Context, deletedInstructionToString }

  override def visitProcedureDefinition(proc: ProcedureDefinition)(c: Context): Context = {
    val position = AstPath(AstPath.Proc(proc.procedure.name.toUpperCase))
    super.visitProcedureDefinition(proc)(c.appendText(c.wsMap.leading(position)))
      .appendText(c.wsMap.backMargin(position))
      .appendText(c.wsMap.trailing(position))
  }

  override def visitCommandBlock(block: CommandBlock, position: AstPath)(implicit c: Context): Context = {
    def beginSyntheticBlock(ws: WhitespaceMap)(p: AstPath): String = ws.leading(p)
    def closeSyntheticBlock(ws: WhitespaceMap)(p: AstPath): String = ws.backMargin(p)

    if (block.synthetic && block.statements.stmts.isEmpty)
      c
    else if (block.synthetic)
      visitBlock(block, position, c1 => super.visitCommandBlock(block, position)(c1),
        beginSyntheticBlock _, closeSyntheticBlock _)
    else
      visitBlock(block, position, c1 => super.visitCommandBlock(block, position)(c1))
  }

  override def visitReporterBlock(block: ReporterBlock, position: AstPath)(implicit c: Context): Context = {
    visitBlock(block, position, c1 => super.visitReporterBlock(block, position)(c1))
  }

  private def normalBeginBlock(ws: WhitespaceMap)(p: AstPath): String = ws.leading(p) + "["
  private def normalEndBlock(ws: WhitespaceMap)(p: AstPath): String = ws.backMargin(p) + "]"

  private def visitBlock(block: AstNode, position: AstPath, visit: Context => Context,
    beginBlock: WhitespaceMap => AstPath => String = normalBeginBlock _,
    endBlock:   WhitespaceMap => AstPath => String = normalEndBlock _)
    (implicit c: Context): Context = {
    c.operations.get(position)
      .map(op => op(this, block, position, c.appendText(leadingWhitespace(position))))
      .getOrElse {
        visit(c.appendText(beginBlock(c.wsMap)(position))).appendText(endBlock(c.wsMap)(position))
      }
  }

  override def visitStatement(stmt: Statement, position: AstPath)(implicit c: Context): Context = {
    c.operations.get(position)
      .map(op => op(this, stmt, position, c))
      .getOrElse {
        val ws = leadingWhitespace(position)
        val newContext = c.appendText(ws + c.instructionToString(stmt.command))
        super.visitStatement(stmt, position)(newContext).copy(instructionToString = c.instructionToString)
      }
  }

  override def visitReporterApp(app: ReporterApp, position: AstPath)(implicit c: Context): Context = {
    import org.nlogo.core.prim.{ _reportertask, _reporterlambda }

    c.operations.get(position)
      .map(op => op(this, app, position, c))
      .getOrElse {
        val ws = leadingWhitespace(position)
        (app.reporter.syntax.isInfix, app.reporter) match {
          case (true, i) =>
            val c2 = visitExpression(app.args.head, position, 0)(c)
            app.args.zipWithIndex.tail.foldLeft(c2.appendText(ws + c.instructionToString(i))) {
              case (ctx, (arg, i)) => visitExpression(arg, position, i)(ctx)
            }
          case (false, con: _const) if con.value.isInstanceOf[LogoList] =>
            super.visitReporterApp(app, position)(c.appendText(leadingWhitespace(position) + c.wsMap.content(position)))
          case (false, r: _reportertask) if r.synthetic =>
            super.visitReporterApp(app, position)(c.appendText(leadingWhitespace(position)))
          case (false, r: _reporterlambda) if r.synthetic =>
            super.visitReporterApp(app, position)(c.appendText(leadingWhitespace(position)))
          case (false, _: _reportertask) =>
            val c2 = super.visitReporterApp(app, position)(Context("", c.operations, wsMap = c.wsMap))
            if (c.text.last == ' ')
              c.appendText("[" + c2.text + "]")
            else
              c.appendText(" [" + c2.text + "]")
          case (false, r: _reporterlambda) =>
            val c2 = super.visitReporterApp(app, position)(Context("", c.operations, wsMap = c.wsMap))
            val args = c.wsMap.frontMargin(position)
            if (c.text.last == ' ')
              c.appendText("[" + args + c2.text + "]")
            else
              c.appendText(" [" + args + c2.text + "]")
          case (false, reporter) =>
            super.visitReporterApp(app, position)(c.appendText(ws + c.instructionToString(reporter)))
              .copy(instructionToString = c.instructionToString)
        }
      }
  }

  private def leadingWhitespace(path: AstPath)(implicit c: Context): String = c.wsMap.leading(path)
}
