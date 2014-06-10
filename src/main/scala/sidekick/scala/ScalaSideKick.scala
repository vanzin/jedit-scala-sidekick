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

import javax.swing.Icon
import javax.swing.ImageIcon
import scala.util.control._

import org.gjt.sp.jedit.Buffer
import org.gjt.sp.jedit.EditPlugin
import org.gjt.sp.util.Log

import scalariform.lexer._
import scalariform.parser._

import errorlist.DefaultErrorSource
import sidekick.SideKickParsedData
import sidekick.SideKickParser

class ScalaSideKick extends SideKickParser("scala") {

  private val CLASS_ICON = new ImageIcon(getClass().getResource("class.png"))
  private val TRAIT_ICON = new ImageIcon(getClass().getResource("trait.png"))
  private val OBJECT_ICON = new ImageIcon(getClass().getResource("object.png"))
  private val FIELD_ICON = new ImageIcon(getClass().getResource("/icons/f/field_public_obj.gif"))
  private val DEF_ICON = new ImageIcon(getClass().getResource("/icons/m/methdef_obj.gif"))

  override def parse(buffer: Buffer,
      errs: DefaultErrorSource): SideKickParsedData = {
    try {
      val ast = ScalaParser.parse(buffer.getText())
      val data = new ScalaParsedData(buffer.getName())
      ast.foreach(n => handleNode(data, n, ""))
      data
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        null
    }
  }

  private def handleNode(data: ScalaParsedData, node: AstNode,
      indent: String): Unit = {
    node match {
      case decl: FullDefOrDcl =>
        parseDef(data, decl)

      case tmpl: TmplDef =>
        tmpl.immediateChildren.foreach(
          c => handleNode(data, c,  indent + "  "))

      case n =>
        node.immediateChildren.foreach(
          c => handleNode(data, c, indent + "  "))

    }
  }

  private def parseDef(data: ScalaParsedData, decl: FullDefOrDcl): Unit = {
    var icon: Icon = null
    var name: String = null

    val loop = new Breaks()
    loop.breakable {
      var openBracketCount = 0
      decl.tokens.foreach(t => t.tokenType.name match {
        case "CLASS" =>
          icon = CLASS_ICON

        case "TRAIT" =>
          icon = TRAIT_ICON

        case "VAL" =>
          icon = FIELD_ICON

        case "VAR" =>
          icon = FIELD_ICON

        case "DEF" =>
          icon = DEF_ICON

        case "OBJECT" =>
          icon = OBJECT_ICON

        case "VARID" =>
          if (openBracketCount == 0) {
            name = t.text
            loop.break()
          }

        case "LBRACKET" =>
          openBracketCount += 1

        case "RBRACKET" =>
          openBracketCount -= 1

        case _ =>
      })
    }

    if (name == null || icon == null) {
      val tokens = decl.tokens.map(t => t.text).mkString(" ")
      Log.log(Log.ERROR, this, s"Unrecognized tokens: $tokens")
      return
    }

    val asset = new ScalaAsset(name, decl.firstToken.offset,
      decl.lastToken.offset + decl.lastToken.length, icon)
    data.addAsset(asset)
  }

}

