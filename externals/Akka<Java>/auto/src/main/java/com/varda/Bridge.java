package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.AskPattern;
import com.bmartin.*;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.vavr.control.*;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletionStage;

@JsonInclude(JsonInclude.Include.NON_EMPTY)
public final class Bridge<P extends Protocol> implements CborSerializable, JsonSerializable, java.io.Serializable {
    static int MAX_REGISTER_RETRY = 10;
    static int REGISTER_POLLING_SLEEP_DURATION = 100; // in ms

    @JsonProperty("id")
    UUID id;
    
    @JsonProperty("protocol")
    public P protocol;

    public Bridge (P protocol){
        assert(protocol != null);
        this.id = UUID.randomUUID();
        this.protocol = protocol;
    }

    @JsonCreator 
    public Bridge (P protocol, UUID id){
        assert(protocol != null);
        assert(id != null);

        this.id = id;
        this.protocol = protocol;
    }
    
    //Same key => Same id
    public Bridge (P protocol, String key){
        assert(protocol != null);
        assert(key != null);

        this.id = UUIDCreator.getSha1Uuid(key);
        this.protocol = protocol;
    }

    public UUID get_id(){
        return this.id;
    }
    public String toString(){
        return "Bridge<UUID="+this.id+">";
    }

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof Bridge)) {
            return false;
        }   

        Bridge b = (Bridge) obj;
        return this.id.equals(b.id) && this.protocol.equals(b.protocol);
    }

    public String leftServiceKey(){
        return Bridge.leftServiceKey(this.id);
    } 

    static public String leftServiceKey(UUID id){
        return AbstractSystem.NAME+"_bridge_left_"+id.toString();
    } 

    private Either<Error, Boolean> register(ActorContext context, ActorRef<SpawnProtocol.Command> guardian, ActivationRef a, boolean left){
        ActorRef<ReplacedReceptionist.Command> receptionist = ReplacedReceptionist.getReceptionist(context); 
        receptionist.tell(new ReplacedReceptionist.Register(left ? this.leftServiceKey() : this.rightServiceKey(), a));

        //Wait that for visible registered activation
        for(int i = 0; i < MAX_REGISTER_RETRY; i++){ //10 retry max
            Set<ActivationRef> activations = 
                left ? this.leftActivations(context, guardian).get() : this.rightActivations(context, guardian).get();
            if(activations.contains(a)){
                if(left)
                    context.getLog().debug("leftRegister "+a.toString()+" at left of "+this.id.toString());
                else
                    context.getLog().debug("rightRegister "+a.toString()+" at right of "+this.id.toString());

                return Either.right(true);
            } else {
                try{
                    Thread.sleep(REGISTER_POLLING_SLEEP_DURATION);
                } catch( Exception e){
                    System.out.println(e);
                }
            }
        }

        context.getLog().error("Bridge::not registered, timeout");
        return Either.left(new Error("timeout before seeing its own update"));
    }

    public Either<Error, Boolean> leftRegister(ActorContext context, ActorRef<SpawnProtocol.Command> guardian, ActivationRef a){
        return this.register(context, guardian, a, true); 
    }

    public String rightServiceKey(){
        return Bridge.rightServiceKey(this.id);
    } 
    static public String rightServiceKey(UUID id){
        return AbstractSystem.NAME+"_bridge_right_"+id.toString();
    } 

    public Either<Error, Boolean> rightRegister(ActorContext context, ActorRef<SpawnProtocol.Command> guardian, ActivationRef a){
        return this.register(context, guardian, a, false); 
    }

    public Either<Error, Set<ActivationRef>> activationsAt(ActorContext context, boolean left){
        assert( context != null);
        ActorRef<ReplacedReceptionist.Command> receptionist = ReplacedReceptionist.getReceptionist(context); 

        CompletionStage<Set<ActivationRef>> ask = AskPattern.ask(
            receptionist,
            replyTo -> new ReplacedReceptionist.GetValue(left ? this.leftServiceKey() : this.rightServiceKey(), replyTo),
            Duration.ofSeconds(10),
            context.getSystem().scheduler());

        try{
            // blocking call
            Set<ActivationRef> tmp = ask.toCompletableFuture().get();
            context.getLog().debug("Find at right/left "+tmp.size());
            return Either.right(tmp);
        } catch (Exception e){
            context.getLog().error(e.toString());
            return Either.left(new Error(e.toString()));
        }
    }

    public Either<Error, Set<ActivationRef>> leftActivations(ActorContext context, ActorRef<SpawnProtocol.Command> guardian){
        return activationsAt( context, true);
    }

    public Either<Error, Set<ActivationRef>> rightActivations(ActorContext context, ActorRef<SpawnProtocol.Command> guardian){
        return activationsAt( context, false);
    }

}