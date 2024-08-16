package cloud.simlytics.devs.xmigenerator

import cloud.simlytics.devs.xmigenerator.Generator._

class DevsStreamingAppGenerator(pkg: String, appName: String, experimentalFrame: String, timeType: String) {

def build(): String =
  s"""
     |package ${pkg};
     |
     |import devs.DevsLoggingActor;
     |import devs.RootCoordinator;
     |import devs.msg.DevsMessage;
     |import devs.msg.InitSim;
     |import devs.msg.log.DevsLogMessage;
     |import devs.msg.time.${timeType};
     |import org.apache.pekko.actor.typed.ActorRef;
     |import org.apache.pekko.actor.typed.Behavior;
     |import org.apache.pekko.actor.typed.javadsl.*;
     |
     |public class ${appName} extends AbstractBehavior<${appName}.Command> {
     |
     |    public interface Command{}
     |    public static class ${appName}Start implements Command{}
     |
     |    @Override
     |    public Receive<Command> createReceive() {
     |        ReceiveBuilder<Command> ${lowerFirstLetter(appName)}ReceiveBuilder = newReceiveBuilder();
     |        ${lowerFirstLetter(appName)}ReceiveBuilder.onMessage(${appName}Start.class, this::onStart);
     |        return ${lowerFirstLetter(appName)}ReceiveBuilder.build();
     |    }
     |
     |     private final Behavior<DevsMessage> experimentalFrame;
     |
     |    private ${appName}(ActorContext<Command> context, Behavior<DevsMessage> experimentalFrame) {
     |        super(context);
     |        this.experimentalFrame = experimentalFrame;
     |    }
     |
     |    protected Behavior<Command> onStart(${appName}Start start) {
     |        ActorContext<Command> context = this.getContext();
     |
     |        ActorRef<DevsMessage> ${lowerFirstLetter(experimentalFrame)} = context.spawn(experimentalFrame, "ModelUtils.toLegalActorName(${lowerFirstLetter(experimentalFrame)})");
     |
     |        ActorRef<DevsMessage> rootCoordinator = context.spawn(RootCoordinator.create(
     |                LongSimTime.builder().t(0L).build(), ${lowerFirstLetter(experimentalFrame)}
     |        ), "root");
     |
     |        rootCoordinator.tell(InitSim.builder().time(LongSimTime.builder().t(0L).build()).build());
     |
     |        //context.watch(rootCoordinator);
     |        return Behaviors.same();
     |    }
     |
     |    public static Behavior<Command> create(Behavior<DevsMessage> experimentalFrame) {
     |        return Behaviors.setup(context -> new ${appName}(context, experimentalFrame));
     |    }
     |}
     |
     |""".stripMargin

}
