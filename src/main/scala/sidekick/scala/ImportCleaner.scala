//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                    Version 2, December 2004
//
// Copyright (C) 2014 Marcelo Vanzin <vanza@users.sourceforge.net>
//
// Everyone is permitted to copy and distribute verbatim or modified
// copies of this license document, and changing it is allowed as long
// as the name is changed.
//
//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  0. You just DO WHAT THE FUCK YOU WANT TO.
package sidekick.scala;

import scala.collection.mutable
import scala.util.Try

import org.gjt.sp.jedit.View
import org.gjt.sp.util.Log

import scalariform.lexer._
import scalariform.parser._

class ImportCleaner(view: View) {

  private val allImports =
      new mutable.HashMap[String, ImportEntry]()
  private val clauses = new mutable.ListBuffer[ImportEntry]()

  def run(): Unit = Try {
    val buffer = view.getBuffer()
    val ast = ScalaParser.parse(buffer.getText())
    ast.foreach(handleNode)

    val unused = clauses.filter(_.hasUnused())
    if (!unused.isEmpty) {
      buffer.writeLock()
      try {
        var offset = 0

        unused.foreach { entry =>
          val clause = entry.clause
          val start = clause.firstToken.offset
          // +1 to cover the new line. Doesn't work with \r\n.
          val end = clause.lastToken.offset + clause.lastToken.length + 1

          buffer.remove(start + offset, end - start)
          if (entry.hasUsed()) {
            val replacement = makeImport(entry.clause, entry.names)
            buffer.insert(start + offset, replacement)
            offset += (replacement.length() - end + start)
          } else {
            // Clean up any leading whitespace in the line removed.
            val text = buffer.getText()
            val importStart = start + offset - 1
            var previousLineEnd = importStart
            while (previousLineEnd > 0 &&
              text.charAt(previousLineEnd) != '\r' &&
              text.charAt(previousLineEnd) != '\n') {
              previousLineEnd -= 1
            }
            if (previousLineEnd < importStart) {
              buffer.remove(previousLineEnd + 1,
                importStart - previousLineEnd)
              offset -= (importStart - previousLineEnd)
            }
            offset -= (end - start)
          }
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
          throw e
      } finally {
        buffer.writeUnlock()
      }
    }
  }

  private def handleNode(node: AstNode): Unit = node match {
    case clause: ImportClause =>
      addImport(clause)

    case expr: ExprElement =>
      expr.tokens.foreach(matchImports)
      expr.immediateChildren.foreach(handleNode)

    case n =>
      n.immediateChildren.foreach(handleNode)
  }

  private def addImport(clause: ImportClause) = {
    val names = clause.importExpr match {
      case Expr(contents) =>
        val name = contents.last.firstToken.text
        val isScalaLanguage = (contents.size >= 3 &&
          contents(0).firstToken.text == "scala" &&
          contents(2).firstToken.text == "language")
        if (name != "_" && !isScalaLanguage) {
          val map = new mutable.HashMap[String, String]()
          map += (name -> name)
          map
        } else {
          new mutable.HashMap[String, String]()
        }

      case BlockImportExpr(_, selectors) =>
        collectNames(selectors)
    }

    if (!names.isEmpty) {
      val entry = new ImportEntry(clause, names)
      clauses += entry
      entry.names.keys.foreach { x => allImports += (x -> entry) }
    }
  }

  private def collectNames(selectors: ImportSelectors):
    mutable.Map[String, String] = {
    val names = new mutable.LinkedHashMap[String, String]()

    val first = selectors.firstImportSelector.contents.head
    val last = selectors.firstImportSelector.contents.last
    names += (last.firstToken.text -> first.firstToken.text)

    selectors.otherImportSelectors.foreach { case (token, expr) =>
      names += (expr.lastToken.text -> expr.firstToken.text)
    }

    names
  }

  private def matchImports(token: Token) = {
    val text = token.text
    if (allImports.contains(text)) {
      allImports(text).names -= text
      allImports -= text
    }
  }

  private def makeImport(clause: ImportClause,
    unused: mutable.Map[String, String]) = {
    val ImportClause(imp, expr, others) = clause
    val BlockImportExpr(prefix, selectors) = expr
    val sb = new StringBuilder()

    sb.append(imp.text).append(" ")

    prefix.tokens.foreach { t =>
      sb.append(t.text)
    }

    val names = collectNames(selectors)
      .filter { case (k, v) => !unused.contains(k) }

    def mkImport(k: String, v: String, wrap: Boolean) = {
      if (k == v) {
        k
      } else {
        val imp = s"$v => $k"
        if (wrap) s"{$imp}" else imp
      }
    }

    val text =
      if (names.size == 1) {
        val (k, v) = names.head
        mkImport(k, v, true)
      } else {
        val imports = names
          .map { case (k, v) => mkImport(k, v, false) }
          .mkString(", ")
        s"{$imports}"
      }
    sb.append(text).append("\n").toString()
  }

  private class ImportEntry(val clause: ImportClause,
    val names: mutable.Map[String, String]) {

    private val originalSize = names.size

    def hasUsed() = names.size != originalSize
    def hasUnused() = !names.isEmpty

  }

}
