package com.varda;

import akka.actor.typed.ActorSystem;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;

import java.util.BitSet;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;

import com.google.gson.Gson;

import org.apache.commons.cli.*;

public class SorterActor extends AbstractBehavior<SorterActor.Command> {
	public interface Command extends CborSerializable {}

    public static final ServiceKey<SorterActor.Command> SORTER_SERVICE_KEY =
      ServiceKey.create(SorterActor.Command.class, "sorter");

    private final int side;
    private int[][] res = new int[2][];
    private ActorRef<ResultMessage> upstream;

    public SorterActor(ActorContext<Command> context, int side) {
        super(context);
        this.side = side;
    }

    static public Behavior<Command> create(int side) {
        return Behaviors.setup(context -> {
            context
                .getSystem()
                .receptionist()
                .tell(Receptionist.register(SORTER_SERVICE_KEY, context.getSelf())); 
            return new SorterActor(context, side);
        });
    }
    
    @Override
    public Receive<Command> createReceive() {
        return newReceiveBuilder()
            .onMessage(StartMessage.class, this::onStart)
            .onMessage(ResultMessage.class, this::onResult)
            .build();
    }
    
    private Behavior<Command> onStart(StartMessage start) {
        ActorContext context = getContext();
        this.upstream = start.replyTo;

        if(start.array.length == 1)
            upstream.tell(new ResultMessage(start.array, this.side, start.i, start.warmup, context.getSelf(), start.initTimestamp));
        else{
            int[] left  = Arrays.copyOfRange(start.array, 0, start.array.length / 2);
            int[] right = Arrays.copyOfRange(start.array, start.array.length / 2, start.array.length);

            ActorRef<SorterActor.Command> a = context.spawn(SorterActor.create(0), "a");
            ActorRef<SorterActor.Command> b = context.spawn(SorterActor.create(1), "b");

            a.tell(new StartMessage(left, start.i, start.warmup, context.getSelf(), start.initTimestamp));
            b.tell(new StartMessage(right, start.i, start.warmup, context.getSelf(), start.initTimestamp));
        }
        return Behaviors.same();
    }

    public Behavior<Command> onResult(ResultMessage result){
        this.res[result.side] = result.array;

        if (this.res[0] != null && this.res[1] != null) {
            int[] resultarray = this.merge(this.res[0], this.res[1]);
            upstream.tell(new ResultMessage(resultarray, this.side, result.i, result.warmup, getContext().getSelf(), result.initTimestamp));
        }

        return Behaviors.same();
    }

    public static int[] merge(int[] a, int[] b){
        int[] answer = new int[a.length + b.length];
        int i = a.length - 1, j = b.length - 1, k = answer.length;
        while (k > 0)
            answer[--k] = (j < 0 || (i >= 0 && a[i] >= b[j])) ? a[i--] : b[j--];
        return answer;
    }
}
