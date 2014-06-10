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
import javax.swing.tree.DefaultMutableTreeNode
import scala.collection.mutable.ListBuffer

import sidekick.SideKickParsedData

private class ScalaParsedData(fname: String)
    extends SideKickParsedData(fname) {

  private[scala] def addAsset(asset: ScalaAsset) = {
    root.add(new DefaultMutableTreeNode(asset))
  }

}

