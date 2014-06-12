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
import javax.swing.text.Position
import javax.swing.tree.DefaultMutableTreeNode
import scala.collection.mutable.ListBuffer

import sidekick.Asset

private object AssetType extends Enumeration {
  type AssetType = Value
  val FIELD, DEF, TYPE = Value
}

private class ScalaAsset(name: String,
    private val atype: AssetType.AssetType,
    start: Int,
    end: Int,
    icon: Icon,
    fullName: Option[String] = None)
    extends Asset(name) {

  private val startPos = new PositionImpl(start)
  private val endPos = new PositionImpl(end)
  private val children = new ListBuffer[ScalaAsset]()

  override def getIcon(): Icon = icon

  override def getLongString(): String = fullName.getOrElse(name)

  override def getShortString(): String = name

  override def toString() = s"ScalaAsset($name)"

  override def getStart(): Position = startPos

  override def getEnd(): Position = endPos

  def getTreeNode(): DefaultMutableTreeNode = {
    val comp = (a1: ScalaAsset, a2: ScalaAsset) => {
      if (a1.atype < a2.atype) {
        true
      } else if (a1.atype > a2.atype) {
        false
      } else {
        a1.name.compareTo(a2.name) <= 0
      }
    }

    val node = new DefaultMutableTreeNode(this)
    children.sortWith(comp).foreach(c => node.add(c.getTreeNode()))
    node
  }

  def addChild(asset: ScalaAsset) = children += asset

  private class PositionImpl(offset: Int) extends Position {
    override def getOffset() = offset
  }

}
