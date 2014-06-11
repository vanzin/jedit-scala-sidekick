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
      ast.foreach(n => handleNode(data, n))
      data
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        null
    }
  }

  private def handleNode(data: ScalaParsedData, node: AstNode): Unit = {
    node match {
      case decl: TmplDef =>
        val asset = parseTmpl(decl)
        decl.immediateChildren.foreach(c => handleChild(asset, c, true))
        data.addAsset(asset)

      case n =>
        node.immediateChildren.foreach(
          c => handleNode(data, c))
    }
  }

  private def handleChild(parent: ScalaAsset, node: AstNode,
      parseFields: Boolean): Unit = {
    node match {
      case decl: TmplDef =>
        val declAsset = parseTmpl(decl)
        parent.addChild(declAsset)
        decl.immediateChildren.foreach(
          c => handleChild(declAsset, c, true))

      case fun: FunDefOrDcl =>
        val funAsset = parseFun(fun)
        parent.addChild(funAsset)
        fun.immediateChildren.foreach(
          c => handleChild(funAsset, c, false))

      case field: PatDefOrDcl =>
        if (parseFields) {
          parent.addChild(parseField(field))
        }

      case n =>
        n.immediateChildren.foreach(
          c => handleChild(parent, c, parseFields))
    }
  }

  private def parseTmpl(decl: TmplDef) = {
    var icon: Icon = null
    val loop = new Breaks()
    loop.breakable {
      decl.tokens.foreach(t => t.tokenType.name match {
        case "CLASS" =>
          icon = CLASS_ICON

        case "TRAIT" =>
          icon = TRAIT_ICON

        case "OBJECT" =>
          icon = OBJECT_ICON

        case _ =>
      })
    }
    new ScalaAsset(decl.name.text,
      AssetType.TYPE,
      decl.firstToken.offset,
      decl.lastToken.offset + decl.lastToken.length,
      icon)
  }

  private def parseFun(fun: FunDefOrDcl) = {
    new ScalaAsset(fun.nameToken.text,
      AssetType.DEF,
      fun.firstToken.offset,
      fun.lastToken.offset + fun.lastToken.length,
      DEF_ICON)
  }

  private def parseField(field: PatDefOrDcl) = {
    new ScalaAsset(findName(field.tokens),
      AssetType.FIELD,
      field.firstToken.offset,
      field.lastToken.offset + field.lastToken.length,
      FIELD_ICON)
  }

  private def findName(tokens: Seq[Token]) = {
    var name: String = null
    val loop = new Breaks()
    loop.breakable {
      var openBracketCount = 0
      tokens.foreach(t => t.tokenType.name match {
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
    name
  }

}

