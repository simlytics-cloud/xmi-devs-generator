package cloud.simlytics.devs.xmigenerator


import Generator._

import scala.xml.Node

case class OwnedOperation(name: String, parameters: List[Parameter], result: Parameter, commentOption: Option[String])

class ModelGenerator(className: String, pkg: String, val immutablesPkg: String, timeType: String,
                     propertyVariables: List[Parameter], stateVariables: List[Parameter],
                     inputPorts: List[Parameter], outputPorts: List[Parameter], ownedOperations: List[OwnedOperation],
                     isAbstract: Boolean, abstractParentModel: Option[String], modelCommentOption: Option[String] = None) {

  // This method is not used.  Potentially delete it.  The current implementation builds internal state as a
  //  Modifiable immutable.  This allows JSON serialization of internal state to be used at model initialization.
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
      s"${comment}    public static final Port<${port.parameterType}> ${lowerFirstLetter(port.name)} = " +
        s"new Port<>(\"${lowerFirstLetter(port.name)}\", ${port.parameterType}.class);"

    }.mkString("\n") + "\n"
  }

  def buildHeader() = {
    // Importing a schedule will only be necessary if the buildInternalState method is used.  Currently, it is not.
    val scheduleImport = stateVariables.map(_.parameterType).find(s => s.startsWith("Schedule<")) match {
      case Some(_) => s"\nimport devs.utils.Schedule;\nimport devs.msg.time.${timeType};"
      case None => ""
    }
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
       |import java.util.Optional;
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

  def buildModelStateAccessors(): String = {
    isAbstract match {
      case false =>
    s"""
       |    protected Modifiable${className}State modifiableState() {
       |        return (Modifiable${className}State)modelState;
       |    }
       |
       |    @Override
       |    public Abstract${className}State getModelState() {
       |        if (modelState instanceof Modifiable${className}State) {
       |            return ((Modifiable${className}State)modelState).toImmutable();
       |        } else {
       |            return modelState;
       |        }
       |    }
       |""".stripMargin
      case true => ""
    }
  }

  def buildOutputFunction(): String = {

    s"""    @Override
       |    public Bag outputFunction() {
       |        Bag.Builder bagBuilder = Bag.builder();
       |        bagBuilder.addAllPortValueList(getPendingOutput());
       |        clearPendingOutput();
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
        val result = op.result.comment match {
          case Some(comment) => s"     * @return ${comment}\n"
          case None => ""
        }
        "    /** " +  lines + params + result + "     */\n"
      }.getOrElse("")
      val parameterList: String = op.parameters.map { p =>
        s"${p.parameterType} ${p.name}"
      }.mkString(", ")
      s"${comment}    protected abstract ${op.result.parameterType} ${op.name}(${parameterList});"
    }.mkString("\n\n")
  }


  def buildModel(): String = {

    val (abstractState, modifiableState, properties) = isAbstract match {
      case true => ("S", "S", "P")
      case false => ("Abstract" + className + "State", "Modifiable" + className + "State", className + "Properties")
    }
    val classDefinition = isAbstract match {
      case false => abstractParentModel match {
        case Some(parentModel) => s"public abstract class ${className} extends ${parentModel}<${properties}, ${abstractState}> {"
        case None => s"public abstract class ${className} extends PDEVSModel<${timeType}, ${abstractState}> {"
      }
      case true => s"public abstract class ${className}<P extends Abstract${className}Properties, S extends Abstract${className}State> extends PDEVSModel<LongSimTime, S> {"
    }
    val superCall = abstractParentModel match {
      case Some(parentModel) => s"super(initialState, identifier, properties)"
      case None => "super(initialState, identifier)"
    }

      s"""
        |${buildHeader()}
        |${classDefinition}
        |
        |public static String modelIdentifier = "${lowerFirstLetter(className)}";
        |${buildPorts()}
        |
        |    protected ${properties} properties;
        |
        |    public ${className}(${modifiableState} initialState, String identifier, ${properties} properties) {
        |        ${superCall};
        |        this.properties = properties;
        |    }
        |
        |${buildOperations()}
        |${buildModelStateAccessors()}
        |${buildHasPendingOutput()}
        |${buildGetPendingOutput()}
        |${buildClearPendingOutput()}
        |${buildOutputFunction()}
        |}
        |""".stripMargin

  }

  def buildTestClass(): String = {
    val (internalState, properties) = isAbstract match {
      case true => ("Abstract" + className + "State", "Abstract" + className + "Properties")
      case false => ("Modifiable" + className + "State", className + "Properties")
    }
    s"""
       |package ${pkg};
       |
       |public abstract class Abstract${className}Test {
       |
       |    protected abstract ${internalState} buildInitialState();
       |    protected abstract ${properties} buildProperties();
       |    protected abstract ${className} buildModel();
       |}
       |""".stripMargin
  }
}
