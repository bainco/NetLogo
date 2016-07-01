// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.fileformat

import java.net.URI

import org.nlogo.api.{ AutoConvertable, ModelFormat }
import org.nlogo.core.Model
import org.nlogo.core.model.WidgetReader

class NLogoThreeDFormat(modelConverter: (Model, Seq[AutoConvertable]) => Model)
  extends ModelFormat[Array[String], NLogoThreeDFormat]
  with AbstractNLogoFormat[NLogoThreeDFormat] {
    val is3DFormat = true
    def name: String = "nlogo3d"
    override def widgetReaders =
      Map[String, WidgetReader]("GRAPHICS-WINDOW" -> ThreeDViewReader)
  }
