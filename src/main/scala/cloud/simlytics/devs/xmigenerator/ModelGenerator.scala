package cloud.simlytics.devs.xmigenerator


import Generator._

import scala.xml.Node

case class OwnedOperation(name: String, parameters: List[Parameter], result: Parameter, commentOption: Option[String])

class ModelGenerator(className: String, pkg: String, val immutablesPkg: String, timeType: String,
                     propertyVariables: List[Parameter], stateVariables: List[Parameter],
                     inputPorts: List[Parameter], outputPorts: List[Parameter], ownedOperations: List[OwnedOperation],
                     modelCommentOption: Option[String] = None) {

  def buildInternalState(variables: List[Parameter], internalClassName: String): String = {

    val vars = variables.map { v =>
      s"        protected ${v.parameterType} ${v.name};"
    }.mkString("\n")

    val constructorVars = variables.map { v =>
      s"${v.parameterType} ${v.name}"
    }.mkString(", ")
    val constructorBody = variables.map { v =>
      s"            this.${v.name} = ${v.name};"
    }.mkString("\n")
    s"""
       |    public static class ${internalClassName} {
       |${vars}
       |        public ${internalClassName}($constructorVars) {
       |${constructorBody}
       |        }
       |    }
       |""".stripMargin
  }

  def buildPorts(): String = {
    val ports = inputPorts ++ outputPorts
    ports.map { port =>
      val comment = buildComment(port.comment)
      s"${comment}    public static Port<${port.parameterType}> ${lowerFirstLetter(port.name)} = " +
        s"new Port<>(\"${lowerFirstLetter(port.name)}\");"

    }.mkString("\n") + "\n"
  }

  def buildHeader() = {
    s"""
       |package ${pkg};
       |
       |import ${immutablesPkg}.*;
       |import devs.Port;
       |import devs.msg.Bag;
       |import devs.msg.PortValue;
       |import devs.msg.time.${timeType};
       |import devs.PDEVSModel;
       |import java.util.List;
       |import java.util.ArrayList;
       |""".stripMargin
  }

  def buildClearPendingOutput(): String = {
    val portBlocks: String = outputPorts.map { outputPort =>
      s"""        modelState.getPending${upperFirstLetter(outputPort.name)}Out().clear();
         |""".stripMargin
    }.mkString("")
    s"""    protected boolean clearPendingOutput() {
       |${portBlocks}
       |        return false;
       |    }
       |""".stripMargin
  }

  def buildHasPendingOutput(): String = {
    val portBlocks: String = outputPorts.map { outputPort =>
      s"""        if (!modelState.getPending${upperFirstLetter(outputPort.name)}Out().isEmpty()) {
         |            return true;
         |        }
         |""".stripMargin
    }.mkString("")
    s"""    protected boolean hasPendingOutput() {
       |${portBlocks}
       |        return false;
       |    }
       |""".stripMargin
  }

  def buildGetPendingOutput(): String = {
    val portBlocks: String = outputPorts.map { outputPort =>
      s"""        for (${outputPort.parameterType} ${lowerFirstLetter(outputPort.parameterType)} : modelState.getPending${upperFirstLetter(outputPort.name)}Out()) {
         |            pendingOutputs.add(${className}.${lowerFirstLetter(outputPort.name)}.createPortValue(${lowerFirstLetter(outputPort.parameterType)}));
         |        }
         |""".stripMargin
    }.mkString("\n")
    s"""    protected List<PortValue<?>> getPendingOutput() {
       |        List<PortValue<?>> pendingOutputs = new ArrayList<>();
       |${portBlocks}
       |        return pendingOutputs;
       |    }
       |""".stripMargin
  }

  def buildOutputFunction(): String = {

    s"""    @Override
       |    protected Bag outputFunction() {
       |        Bag.Builder bagBuilder = Bag.builder();
       |        bagBuilder.addAllPortValueList(getPendingOutput());
       |        return bagBuilder.build();
       |    }
       |""".stripMargin
  }

  def buildOperations(): String = {
    ownedOperations.map { op =>
      val comment: String = op.commentOption.map {comment =>
        val lines = comment.split("\n").mkString("\n     * ") + "\n"
        val params = op.parameters.map { p =>
          s"     * @param ${p.name} ${p.comment.getOrElse("")}\n"
        }.mkString("")
        "    /** " +  lines + params + "     */\n"
      }.getOrElse("")
      val parameterList: String = op.parameters.map { p =>
        s"${p.parameterType} ${p.name}"
      }.mkString(", ")
      s"${comment}    protected abstract ${op.result.parameterType} ${op.name}(${parameterList});"
    }.mkString("\n\n")
  }


  def buildModel(): String = {

    val internalState = "Modifiable" + className + "State"

      s"""
        |${buildHeader()}
        |public abstract class ${className} extends PDEVSModel<${timeType}, ${internalState}> {
        |
        |public static String modelIdentifier = "${lowerFirstLetter(className)}";
        |${buildPorts()}
        |
        |    protected ${className}Properties properties;
        |
        |    public ${className}(${internalState} initialState, String identifier, ${className}Properties properties) {
        |        super(initialState, identifier);
        |        this.properties = properties;
        |    }
        |
        |${buildOperations()}
        |${buildHasPendingOutput()}
        |${buildGetPendingOutput()}
        |${buildClearPendingOutput()}
        |${buildOutputFunction()}
        |}
        |""".stripMargin

  }
}
