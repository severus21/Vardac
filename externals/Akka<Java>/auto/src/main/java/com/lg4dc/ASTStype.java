package com.lg4dc;

import io.vavr.*;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.lg4dc.timers.HBSessionTimer;

import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.TimerScheduler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.As;
import com.fasterxml.jackson.annotation.JsonTypeName;

import com.lg4dc.timers.*;
import com.bmartin.*;

// Session types as values
public final class ASTStype {
    public static final class MsgT implements CborSerializable{
        //We use String here otherwise Jackson is not able to correctly reconstruct subtype (except if we provide a list of all subtypes)
        public String value;

        @JsonCreator
        public MsgT(@JsonProperty("value") String value){
            this.value = value;
        }

        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }

            if (!(obj instanceof MsgT)) {
                return false;
            }   

            MsgT b = (MsgT) obj;
            return this.value.equals(b.value);
        }
        public String toString(){
            return "MsgT<"+this.value+">";
        }
    }
    public static enum TimerKind {
        LB, HB
    }
    public static class TimerHeader implements CborSerializable{


        public TimerKind kind;
        public String timer_name;
        public Integer trigger_value;

        public TimerHeader (TimerKind kind, String timer_name, Integer trigger_value) {
            this.kind = kind;
            this.timer_name = timer_name;
            this.trigger_value = trigger_value;
        }

        public static void apply_headers (ActorContext context, TimerScheduler contextTimers, Set<UUID> frozen_sessions, Set<UUID> dead_sessions, Session s){
            if(s.st.continuations.size() > 0){
                List<TimerHeader> headers = s.st.continuations.get(0)._2;
                for (ASTStype.TimerHeader timer : headers) {
                    context.getLog().debug(String.format("timer %s trigger value = %d", timer.timer_name, timer.trigger_value));
                   
                    // Time name is unique for a given (timer_name, session_id)
                    String specialized_timer_name = timer.timer_name + "_" + s.session_id.toString();


                    if(contextTimers.isTimerActive(specialized_timer_name))
                        contextTimers.cancel(specialized_timer_name);

                    if(timer.kind == TimerKind.LB){
                        frozen_sessions.add(s.session_id);
                        contextTimers.startSingleTimer(
                            specialized_timer_name, 
                            new LBSessionTimer(s.session_id, s.right) , 
                            Duration.ofMillis(timer.trigger_value) 
                        );
                    }else{
                        contextTimers.startSingleTimer(
                            specialized_timer_name, 
                            new HBSessionTimer(s.session_id, s.right) , 
                            Duration.ofMillis(timer.trigger_value) 
                        );
                    }
                }
            }
        }
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, 
      include = As.PROPERTY, property = "type") @JsonSubTypes({
         
      @JsonSubTypes.Type(value = End.class, name = "End"),
      @JsonSubTypes.Type(value = Send.class, name = "Send"),
      @JsonSubTypes.Type(value = Receive.class, name = "Receive"),
      @JsonSubTypes.Type(value = Branch.class, name = "Branch"),
      @JsonSubTypes.Type(value = Select.class, name = "Select")
   })
    public static class Base implements CborSerializable{
        /*
            Empty continuation <=> End
            Send and Receive [("", continuation)]
            Choice and Select [ (label, continuaiton_label); ....]
            Recursive ????? TODO
        */
        public List<Tuple3<MsgT, List<TimerHeader>, Base>> continuations = new ArrayList<>();

        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }

            if (!(obj instanceof Base)) {
                return false;
            }   

            Base b = (Base) obj;
            //Headers are not part of equals since equals reason on the types
            //And headers are just annotation to direct the execution
            List<Tuple2<MsgT, Base>> r1 = this.continuations.stream().map(continuation -> Tuple.of(continuation._1, continuation._3)).collect(Collectors.toList());
            List<Tuple2<MsgT, Base>> r2 = b.continuations.stream().map(continuation -> Tuple.of(continuation._1, continuation._3)).collect(Collectors.toList());
            return r1.equals(r2); 
        }

        public String toString(){
            return "Base";
        }

    }

    @JsonTypeName("End")
    public static final class End extends Base {
        public End () {
            assert(this.continuations.isEmpty());
        }
        public String toString(){
            return "End";
        }
    }

    @JsonTypeName("Send")
    public static final class Send extends Base {
        public MsgT msg_type;
        public Send(MsgT msg_type, List<TimerHeader> timers, Base continuation){
            this.msg_type = msg_type;
            this.continuations.add(new Tuple3(msg_type, timers, continuation));

            assert(!this.continuations.isEmpty());
        }
        public String toString(){
            return "Send<"+this.msg_type.getClass().toString()+", "+this.continuations.toString()+">";
        }
    }

    @JsonTypeName("Receive")
    public static final class Receive extends Base {
        public MsgT msg_type;
        public Receive(MsgT msg_type, List<TimerHeader> timers, Base continuation){
            this.msg_type = msg_type;
            this.continuations.add(new Tuple3(msg_type, timers, continuation));

            assert(!this.continuations.isEmpty());
        }

        public String toString(){
            return "Receive<"+this.msg_type.getClass().toString()+", "+this.continuations.toString()+">";
        }
    }

    @JsonTypeName("Branch")
    public static final class Branch extends Base {
        public Branch(List<Tuple3<MsgT, List<TimerHeader>, Base>> continuations){
            this.continuations = continuations;

            assert(!this.continuations.isEmpty());
        }
        public String toString(){
            return "Branch<"+this.continuations.toString()+">";
        }
    }

    @JsonTypeName("Select")
    public static final class Select extends Base {
        public Select(List<Tuple3<MsgT, List<TimerHeader>, Base>> continuations){
            this.continuations = continuations;

            assert(this.continuations.isEmpty());
        }
        public String toString(){
            return "Select<"+this.continuations.toString()+">";
        }
    }
    // TODO recursion
}