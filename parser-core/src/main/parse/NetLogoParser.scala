// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.parse

import org.nlogo.core.{ CompilationOperand, Dialect, DummyExtensionManager, ExtensionManager,
  FrontEndInterface, FrontEndProcedure, NetLogoCore, ProcedureDefinition,
  ProcedureSyntax, StructureResults, TokenizerInterface },
  FrontEndInterface.ProceduresMap


// trait for methods shared by FrontEnd and AstRewriter, both of which parse NetLogo code
trait NetLogoParser {
  def tokenizer: TokenizerInterface

  def basicParse(compilationOperand: CompilationOperand): (Seq[ProcedureDefinition], StructureResults) = {
    import compilationOperand.{ extensionManager, oldProcedures }
    val structureResults = StructureParser.parseSources(tokenizer, compilationOperand)
    val globallyUsedNames =
      StructureParser.usedNames(structureResults.program,
        oldProcedures ++ structureResults.procedures)

    val newTopLevelProcedures = (structureResults.procedures -- oldProcedures.keys)

    val topLevelDefs = newTopLevelProcedures.values
      .map(parseProcedure(structureResults, globallyUsedNames, oldProcedures, extensionManager)).toSeq
    (topLevelDefs, structureResults)
  }

  def parseProcedure(
    structureResults:  StructureResults,
    globallyUsedNames: Map[String, SymbolType],
    oldProcedures:     ProceduresMap,
    extensionManager:  ExtensionManager)(procedure: FrontEndProcedure): ProcedureDefinition = {
    val rawTokens = structureResults.procedureTokens(procedure.name)
    val usedNames = globallyUsedNames ++ procedure.args.map(_ -> SymbolType.LocalVariable)
    // on LetNamer vs. Namer vs. LetScoper, see comments in LetScoper
    val namedTokens = {
      val letNamedTokens = TransformableTokenStream(rawTokens.iterator, LetNamer)
      val namer =
        new Namer(structureResults.program,
          oldProcedures ++ structureResults.procedures,
          procedure, extensionManager)
      namer.validateProcedure()
      val namedTokens = TransformableTokenStream(letNamedTokens, namer)
      val letScoper = new LetScoper(usedNames, namedTokens)
      // we map unknown idents to symbols and ExpressionParser errors as appropriate
      val letScopedStream = TransformableTokenStream(namedTokens, letScoper)
      val lambdaScopedStream = TransformableTokenStream(letScopedStream, new LambdaScoper(usedNames))
      lambdaScopedStream.map {
        case t @ org.nlogo.core.Token(_, org.nlogo.core.TokenType.Reporter, l: org.nlogo.core.prim._unknownidentifier) =>
          t.refine(org.nlogo.core.prim._symbol())
        case t => t
      }
    }
    ExpressionParser(procedure, namedTokens)
  }

  def findProcedurePositions(source: String, dialectOption: Option[Dialect]): Map[String, ProcedureSyntax] = {
    val dialect = dialectOption.getOrElse(NetLogoCore)
    val tokens = tokenizer.tokenizeString(source).map(Namer.basicNamer(dialect, new DummyExtensionManager))
    StructureParser.findProcedurePositions(tokens.toSeq)
  }
}
