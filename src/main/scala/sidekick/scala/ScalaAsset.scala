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

import sidekick.Asset

private class ScalaAsset(name: String, start: Int, end: Int, icon: Icon)
    extends Asset(name) {

  private val startPos = new PositionImpl(start)
  private val endPos = new PositionImpl(end)
  private val treeNode = new DefaultMutableTreeNode(this)

  override def getIcon(): Icon = icon

  override def getLongString(): String = name

  override def getShortString(): String = name

  override def toString() = s"ScalaAsset($name)"

  override def getStart(): Position = startPos

  override def getEnd(): Position = endPos

  def getTreeNode() = treeNode

  def addChild(asset: ScalaAsset) = treeNode.add(asset.getTreeNode())

  private class PositionImpl(offset: Int) extends Position {
    override def getOffset() = offset
  }

}

