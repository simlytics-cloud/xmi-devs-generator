package cloud.simlytics.devs.xmigenerator

case class ItemFlow(fromModel: String, fromPort: String, fromPortType: String, toModel: String, toPort: String, toPortType: String)

class CouplingsGenerator(val pkg: String, val basePackage: String, val immutablesPkg: String, val coupledModelName: String, flows: List[ItemFlow], val subordinateCoupledModels: List[String]) {
  val coupledModelImports: String = subordinateCoupledModels.map { m =>
    s"import ${pkg}.${m.toLowerCase}.${m};"
  }.mkString("\n")


  def buildOutputCouplings(): String = {
    s"""
       |${buildOutputCouplingHeader()}
       |public class ${coupledModelName}OutputCouplingHandler extends OutputCouplingHandler {
       |
       |    public ${coupledModelName}OutputCouplingHandler() {
       |        super(Optional.empty(), Optional.empty(), Optional.empty());
       |    }
       |
       |    @Override
       |    public void handlePortValue(String sender, PortValue<?> portValue,
       |                                Map<String, List<PortValue<?>>> receiverMap,
       |                                List<PortValue<?>> outputMessages) {
       |
       |${buildOutputCouplingFlows()}
       |    }
       |${buildDetermineTargetModel()}
       |}
       |""".stripMargin
  }

  def buildInputCouplings(): String = {
    s"""
       |${buildInputCouplingHeader()}
       |public class ${coupledModelName}InputCouplingHandler extends InputCouplingHandler {
       |
       |    public ${coupledModelName}InputCouplingHandler() {
       |      super(Optional.empty());
       |    }
       |
       |    @Override
       |    public void handlePortValue(PortValue<?> portValue, Map<String, List<PortValue<?>>> receiverMap) {
       |
       |${buildInputCouplingFlows()}
       |    }
       |${buildDetermineTargetModel()}
       |}
       |""".stripMargin
  }

  def buildDetermineTargetModel(): String = {
    val cases = flows.map { flow =>
      s"            case \"${flow.toPort}\" -> ${flow.toModel}.modelIdentifier;"
    }.distinct.mkString("\n")
    if (cases.isEmpty) {
      ""
    } else {
      s"""
         |    protected String determineTargetModel(PortValue<?> fromPortValue) {
         |        return switch (fromPortValue.getPortIdentifier()) {
         |${cases}
         |            default -> throw new IllegalArgumentException(
         |                    "Could not identify target model from PortValue with identifier " +
         |                            fromPortValue.getPortIdentifier());
         |        };
         |    }
         |""".stripMargin
    }
  }

  def buildOutputCouplingFlows(): String = {
    flows.filter(_.fromModel != coupledModelName)  // Filter out inputs to coupled model
      .map { flow =>
      val flowRouting: String = (flow.toModel == coupledModelName) match {
        case true =>
          "outputMessages.add(flowPortValue);"  // Add flow to outputs
        case false => // Route to correct internal model via receiverMap
          s"addInputPortValue(flowPortValue, determineTargetModel(portValue), receiverMap);"
      }
      s"""        if (portValue.getPortIdentifier().equals(${flow.fromModel}.${flow.fromPort}.getPortIdentifier())) {
         |            PortValue<${flow.toPortType}> flowPortValue = ${flow.toModel}.${flow.toPort}.createPortValue(
         |                ${flow.fromModel}.${flow.fromPort}.getValue(portValue));
         |            ${flowRouting}
         |        }
         |""".stripMargin
    }.mkString("\n")
  }

  def buildInputCouplingFlows(): String = {
    flows.filter(_.fromModel == coupledModelName)
      .map { flow =>
        s"""        if (portValue.getPortIdentifier().equals(${flow.fromModel}.${flow.fromPort}.getPortIdentifier())) {
           |            PortValue<${flow.toPortType}> flowPortValue = ${flow.toModel}.${flow.toPort}.createPortValue(
           |                ${flow.fromModel}.${flow.fromPort}.getValue(portValue));
           |            addInputPortValue(flowPortValue, determineTargetModel(portValue), receiverMap);
           |        }
           |""".stripMargin
      }.mkString("\n")
  }

  def buildOutputCouplingHeader(): String = {
    s"""
       |package ${pkg};
       |
       |import ${immutablesPkg}.*;
       |import devs.OutputCouplingHandler;
       |import devs.msg.PortValue;
       |
       |import java.util.List;
       |import java.util.Map;
       |import java.util.Optional;
       |${coupledModelImports}
       |import ${basePackage}.immutables.*;
       |
       |""".stripMargin
  }

  def buildInputCouplingHeader(): String = {
    s"""
       |package ${pkg};
       |
       |import devs.InputCouplingHandler;
       |import devs.msg.PortValue;
       |
       |import java.util.List;
       |import java.util.Map;
       |import java.util.Optional;
       |${coupledModelImports}
       |import ${basePackage}.immutables.*;
       |
       |""".stripMargin
  }

}
