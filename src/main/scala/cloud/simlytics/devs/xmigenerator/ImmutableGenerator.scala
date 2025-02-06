package cloud.simlytics.devs.xmigenerator

import scala.xml.Node
import Generator._
import XmlParser._

class ImmutableGenerator(val className: String, val pkg: String, val immutablesPkg: String, val otherPackages: List[String], val variables: List[Parameter], val timeType: String, val isSimState: Boolean = false, superclass: Option[String] = None) {

  def buildHeader(): String = {
    val immutable: String = isSimState match {
      case false => "@Value.Immutable"
      case true => "@Value.Immutable\n@Value.Modifiable"
    }
    val scheduleImport = variables.map(_.parameterType).find(s => s.startsWith("Schedule<")) match {
      case Some(_) => s"\nimport devs.utils.Schedule;\nimport devs.msg.time.${timeType};"
      case None => ""
    }
    s"""
       |package ${pkg};
       |
       |import ${immutablesPkg}.*;
       |${otherPackages.map(p => "import " + p + ".*;").mkString("\n")}
       |import java.util.List;
       |import java.util.Map;
       |import java.util.Optional;
       |import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
       |import com.fasterxml.jackson.databind.annotation.JsonSerialize;
       |import org.immutables.value.Value;
       |
       |import javax.annotation.Nullable;${scheduleImport}
       |
       |${immutable}
       |@JsonSerialize(as = ${className}.class)
       |@JsonDeserialize(as = ${className}.class)
       |""".stripMargin
  }

  def buildMethods(): String = {
    variables.map { v =>
      val comment = buildComment(v.comment)
      s"${comment}    public abstract ${v.parameterType} get${upperFirstLetter(v.name)}();"
    }.mkString("\n")
  };

  def buildUpdateState(): String = {
    isSimState match {
      case false => ""
      case true =>
    s"""    public ${className} updateState(${className} ${lowerFirstLetter(className)}) {
       |        ${className} updated${className} = ${className}.copyOf(this);
       |${
      variables.map { v =>
        val condition =         s"        if (${lowerFirstLetter(className)}.get${upperFirstLetter(v.name)}() != null) {\n"
        val updateLine =
            s"            updated${className} = updated$className.with${upperFirstLetter(v.name)}(${lowerFirstLetter(className)}.get${upperFirstLetter(v.name)}());\n        }"
        condition + updateLine + "\n"
      }.mkString("\n")}
       |        return updated${className};
       |    }
       |""".stripMargin
    }
  }

  def build(): String = {
    s"""
       |${buildHeader()}
       |public abstract class Abstract${className} ${superclass.map(sc => s"extends ${sc}").getOrElse("")} {
       |
       |${buildMethods()}
       |}
       |""".stripMargin
  }
}
