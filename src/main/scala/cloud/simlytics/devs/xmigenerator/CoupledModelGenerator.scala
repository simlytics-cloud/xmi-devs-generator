package cloud.simlytics.devs.xmigenerator

import cloud.simlytics.devs.xmigenerator.Generator._

class CoupledModelGenerator(pkg: String, val immutablesPkg: String, val coupledModelName: String, timeType: String,
                            val ports: List[Parameter],
                            subordinateAtomicModels: List[String], subordinateCoupledModels: List[String]) {

  def buildHeader(): String = {
    val subordinateModelsImports: String = subordinateCoupledModels.map { subordinateCoupledModel =>
      s"import ${pkg}.${subordinateCoupledModel.toLowerCase}.*;"
    }.mkString("\n")
    s"""
       |package ${pkg};
       |
       |import ${immutablesPkg}.*;
       |import org.apache.pekko.actor.typed.ActorRef;
       |import org.apache.pekko.actor.typed.javadsl.ActorContext;
       |import devs.PDevsCoordinator;
       |import devs.PDevsCouplings;
       |import devs.Port;
       |import devs.msg.DevsMessage;
       |import devs.msg.time.${timeType};
       |import java.util.Map;
       |${subordinateModelsImports}
       |""".stripMargin
  }

  def buildFactoryHeader(): String = {
    val subordinateModelsImports: String = subordinateCoupledModels.map { subordinateCoupledModel =>
      s"import ${pkg}.${subordinateCoupledModel.toLowerCase}.*;"
    }.mkString("\n")
    s"""
       |package ${pkg};
       |
       |import ${immutablesPkg}.*;
       |import org.apache.pekko.actor.typed.ActorRef;
       |import org.apache.pekko.actor.typed.Behavior;
       |import org.apache.pekko.actor.typed.javadsl.Behaviors;
       |import devs.*;
       |import devs.msg.DevsMessage;
       |import devs.msg.time.${timeType};
       |import devs.utils.ModelUtils;
       |import java.util.Collections;
       |import java.util.List;
       |import java.util.Map;
       |import java.util.HashMap;
       |${subordinateModelsImports}
       |""".stripMargin
  }


  def buildModel(): String = {
    s"""
       |${buildHeader()}
       |public class ${coupledModelName} extends PDevsCoordinator<${timeType}> {
       |
       |    public static String modelIdentifier = "${lowerFirstLetter(coupledModelName)}";
       |
       |${buildPorts()}
       |    public ${coupledModelName}(
       |            String modelIdentifier,
       |            Map<String, ActorRef<DevsMessage>> modelsSimulators,
       |            PDevsCouplings couplings,
       |            ActorContext<DevsMessage> context) {
       |        super(modelIdentifier, modelsSimulators, couplings, context);
       |    }
       |}
       |""".stripMargin
  }

  def buildFactory(): String = {
    s"""
       |${buildFactoryHeader()}
       |
       |public abstract class Abstract${coupledModelName}Factory {
       |
       |    protected List<InputCouplingHandler> buildInputCouplings() {
       |        return Collections.singletonList(new ${coupledModelName}InputCouplingHandler());
       |    }
       |    protected List<OutputCouplingHandler> buildOutputCouplings() {
       |        return Collections.singletonList(new ${coupledModelName}OutputCouplingHandler());
       |    }
       |${buildModelInitializationData()}
       |${buildCreate()}
       |}
       |""".stripMargin
  }

  def buildModelInitializationData(): String = {
    subordinateAtomicModels.map { m =>
      s"""    protected abstract List<${m}> build${m}s();
         |""".stripMargin
    }.mkString("\n") +
      subordinateCoupledModels.map { cm =>
        s"""
           |    protected abstract Abstract${cm}Factory build${cm}Factory();
           |""".stripMargin
      }.mkString("\n")
  }

  def buildCreate(): String = {
    s"""
       |    public Map<String, Behavior<DevsMessage>> create(String parentIdentifier) {
       |        ${timeType} t0 = ${timeType}.builder().t(0L).build();
       |        Map<String, ActorRef<DevsMessage>> modelSimulators = new HashMap<>();
       |        PDevsCouplings couplings = new PDevsCouplings(
       |                    buildInputCouplings(), buildOutputCouplings());
       |        return Collections.singletonMap(${coupledModelName}.modelIdentifier, Behaviors.setup(context -> {
       |${subordinateAtomicModels.map(m => {
      s"                build${m}s().stream().forEach(devsModel -> {\n" +
      s"                    ActorRef<DevsMessage> atomicModelRef = context.spawn(PDevsSimulator.create(\n" +
      s"                        devsModel, t0), ModelUtils.toLegalActorName(devsModel.getModelIdentifier()));\n" +
      s"                   context.watch(atomicModelRef);\n" +
      s"                   modelSimulators.put(devsModel.getModelIdentifier(), atomicModelRef);\n" +
      s"                });"}
        ).mkString("\n")}
       |${subordinateCoupledModels.map(m => {
      s"                build${m}Factory().create(${coupledModelName}.modelIdentifier).entrySet().stream().forEach(entry -> {\n" +
      s"                    ActorRef<DevsMessage> coupledModelRef = context.spawn(entry.getValue(), ModelUtils.toLegalActorName(entry.getKey()));\n" +
      s"                    context.watch(coupledModelRef);\n" +
      s"                    modelSimulators.put(entry.getKey(), coupledModelRef);\n" +
      s"                });"}
        ).mkString("\n")}
       |            return new ${coupledModelName}(${coupledModelName}.modelIdentifier, parentIdentifier, modelSimulators, couplings, context);
       |        }));
       |    }
       |
       |""".stripMargin
  }

  def buildPorts(): String = {
    ports.map { port =>
      s"${buildComment(port.comment)}    public static final Port<${port.parameterType}> ${lowerFirstLetter(port.name)} = " +
        s"new Port<>(\"${lowerFirstLetter(port.name)}\", ${port.parameterType}.class);"

    }.mkString("\n") + "\n"
  }

}
