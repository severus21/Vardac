package com.lg4dc;

import io.vavr.*;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.lg4dc.timers.HBSessionTimer;

import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.TimerScheduler;

import com.lg4dc.timers.*;
import com.bmartin.*;

// Session types as values
public final class ASTStype {
    public static final class MsgT {
        public Object value;
        public MsgT(Object value){
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
    }
    public static enum TimerKind {
        LB, HB
    }
    public static class TimerHeader {


        public TimerKind kind;
        public String timer_name;
        public Integer trigger_value;

        public TimerHeader (TimerKind kind, String timer_name, Integer trigger_value) {
            this.kind = kind;
            this.timer_name = timer_name;
            this.trigger_value = trigger_value;
        }

        public void apply_headers (ActorContext<CborSerializable> context, TimerScheduler<CborSerializable> contextTimers, UUID session_id, List<TimerHeader> headers){
            for (ASTStype.TimerHeader timer : headers) {
                if(contextTimers.isTimerActive(timer.timer_name))
                    contextTimers.cancel(timer.timer_name);

                if(timer.kind == TimerKind.LB){
                    contextTimers.startSingleTimer(
                        timer.timer_name, 
                        new LBSessionTimer(session_id, context.getSelf()) , 
                        Duration.ofMillis(timer.trigger_value) 
                    );
                }else{
                    contextTimers.startSingleTimer(
                        timer.timer_name, 
                        new HBSessionTimer(session_id, context.getSelf()) , 
                        Duration.ofMillis(timer.trigger_value) 
                    );
                }
            }

        }
    }

    public static class Base {
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
            return this.continuations.equals(b.continuations); 
        }

    }


    public static final class End extends Base {
        public End () {
            assert(this.continuations.isEmpty());
        }
    }

    public static final class Send extends Base {
        public MsgT msg_type;
        public Send(MsgT msg_type, List<TimerHeader> timers, Base continuation){
            this.msg_type = msg_type;
            this.continuations.add(new Tuple3(msg_type, timers, continuation));

            assert(!this.continuations.isEmpty());
        }
    }
    public static final class Receive extends Base {
        public MsgT msg_type;
        public Receive(MsgT msg_type, List<TimerHeader> timers, Base continuation){
            this.msg_type = msg_type;
            this.continuations.add(new Tuple3(msg_type, timers, continuation));

            assert(!this.continuations.isEmpty());
        }

    }
    public static final class Branch extends Base {
        public Branch(List<Tuple3<MsgT, List<TimerHeader>, Base>> continuations){
            this.continuations = continuations;

            assert(!this.continuations.isEmpty());
        }
    }
    public static final class Select extends Base {
        public Select(List<Tuple3<MsgT, List<TimerHeader>, Base>> continuations){
            this.continuations = continuations;

            assert(this.continuations.isEmpty());
        }
    }
    // TODO recursion
}