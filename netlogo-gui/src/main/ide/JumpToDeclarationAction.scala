// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.ide

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, Action}

import org.nlogo.core.I18N
import org.nlogo.editor.EditorAwareAction

class JumpToDeclarationAction() extends AbstractAction with EditorAwareAction {
  putValue(Action.NAME, I18N.gui.get("tabs.code.rightclick.jumptodeclaration"))
  def actionPerformed(e: ActionEvent): Unit = {
    JumpToDeclaration.jumpToDeclaration(documentOffset, editor)
  }
}
