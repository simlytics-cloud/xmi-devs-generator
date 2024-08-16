package cloud.simlytics.devs.xmigenerator

import Generator._

class EnumerationGenerator(val className: String, val immutablesPkg: String, val values: List[String], val commentOption: Option[String]) {

  def build(): String = {
    s"""
       |package ${immutablesPkg};
       |
       |${buildComment(commentOption)}public enum ${className} {
       |  ${values.mkString(",\n  ")}
       |}
       |""".stripMargin
  }
  
}
