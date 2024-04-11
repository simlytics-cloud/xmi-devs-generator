package cloud.simlytics.devs.xmigenerator

import cloud.simlytics.devs.xmigenerator.Generator.*
import cloud.simlytics.devs.xmigenerator.XmlParser.*

import scala.xml.Node

class AbstractClassGenerator(val className: String, val pkg: String, val immutablesPkg: String, val variables: List[Parameter], superclass: Option[String] = None) {

  def buildHeader(): String = {
    s"""
       |package ${pkg};
       |
       |import ${immutablesPkg}.*;
       |import java.util.List;
       |import java.util.Map;
       |""".stripMargin
  }

  def buildMethods(): String = {
    variables.map { v =>
      val comment = buildComment(v.comment)
      s"${comment}    public abstract ${v.parameterType} get${upperFirstLetter(v.name)}();"
    }.mkString("\n")
  };


  def build(): String = {
    s"""
       |${buildHeader()}
       |public abstract class ${className} {
       |
       |${buildMethods()}
       |}
       |""".stripMargin
  }
}
