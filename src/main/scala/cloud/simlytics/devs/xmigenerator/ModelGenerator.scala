package cloud.simlytics.devs.xmigenerator


import Generator._

import scala.xml.Node

case class OwnedOperation(name: String, parameters: List[String], result: String)

class ModelGenerator(className: String, pkg: String, val immutablesPkg: String, timeType: String,
                     propertyVariables: List[String], stateVariables: List[String],
                     inputPorts: List[String], outputPorts: List[String], ownedOperations: List[OwnedOperation]) {

  def buildInternalState(variables: List[String], internalClassName: String): String = {

    val vars = variables.map { v =>
      s"        protected ${v};"
    }.mkString("\n")

    val constructorVars = variables.map { v =>
      s"${v}"
    }.mkString(", ")
    val constructorBody = variables.map { v =>
      val (_, varName) = splitTypeAndName(v)
      s"            this.${varName} = ${varName};"
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
      val (portType, portName) = splitTypeAndName(port)
      s"    public static Port<${portType}> ${lowerFirstLetter(portName)} = " +
        s"new Port<>(\"${camelToUnderscores(upperFirstLetter(portName)).toUpperCase}\");"

    }.mkString("\n") + "\n"
  }

  def buildHeader() = {
    s"""
       |package ${pkg};
       |
       |import ${immutablesPkg}.*;
       |import devs.Port;
       |import devs.msg.Bag;
       |import devs.msg.time.${timeType};
       |import devs.PDEVSModel;
       |import java.util.List;
       |
       |""".stripMargin
  }

  def buildOutputFunction(): String = {
    val portBlocks: String = outputPorts.map { outputPort =>
      val (portType, portName) = splitTypeAndName(outputPort)
      s"""        for (${portType} ${lowerFirstLetter(portType)} : modelState.getPending${upperFirstLetter(portName)}Out()) {
         |            bagBuilder.addPortValueList(${className}.${lowerFirstLetter(portName)}.createPortValue(${lowerFirstLetter(portType)}));
         |        }
         |        modelState.getPending${upperFirstLetter(portName)}Out().clear();
         |""".stripMargin
    }.mkString("\n")
    s"""    @Override
       |    protected Bag outputFunction() {
       |        Bag.Builder bagBuilder = Bag.builder();
       |${portBlocks}
       |        return bagBuilder.build();
       |    }
       |""".stripMargin
  }

  def buildOperations(): String = {
    ownedOperations.map { op =>
      s"    protected abstract ${op.result} ${op.name}(${op.parameters.mkString(", ")});"
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
        |${buildOutputFunction()}
        |}
        |""".stripMargin

  }
}
