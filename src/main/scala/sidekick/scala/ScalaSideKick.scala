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

import org.gjt.sp.jedit.Buffer
import org.gjt.sp.jedit.EditPlugin

import errorlist.DefaultErrorSource
import sidekick.SideKickParsedData
import sidekick.SideKickParser

class ScalaSideKick extends SideKickParser("scala") {

  override def parse(buffer: Buffer,
      errs: DefaultErrorSource): SideKickParsedData = {
    throw new UnsupportedOperationException()
  }

}

