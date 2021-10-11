package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;
import java.util.Set;
import java.util.Map;
import java.util.List;


import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;


public class Handlers {
    public static void onHBTimer(
        ActorContext context, 
        ActorRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        Set<UUID> dead_sessions, 
        List<Map<UUID, ?>> intermediate_states, 
        HBSessionTimer timerMsg){
        context.getLog().debug("receive HBSessionTimer");
        context.getLog().warn( String.format("Session %s of type ? between [%s] and [%s] has timeout after ? ms", timerMsg.session_id, timerMsg.replyTo.toString(), self.toString()));

        timeout_sessions.add(timerMsg.session_id);
        dead_sessions.add(timerMsg.session_id);

        timerMsg.replyTo.tell(new SessionHasTimeout(timerMsg.session_id, (ActorRef) self));
    } 
    public static void onHasTimeout(
        ActorContext context, 
        ActorRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        Set<UUID> dead_sessions, 
        List<Map<UUID, ?>> intermediate_states, 
        SessionHasTimeout timerMsg){
        context.getLog().debug("receive SessionHasTimeout");

        timeout_sessions.add(timerMsg.session_id);
        dead_sessions.add(timerMsg.session_id);

        timerMsg.replyTo.tell(new AckSessionIsDead(timerMsg.session_id, (ActorRef) self));
    }

    // LB
    private static void onFrozen(
        ActorContext context, 
        ActorRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        Set<UUID> dead_sessions, 
        List<Map<UUID, ?>> intermediate_states, 
        SessionTimer timerMsg) {
        frozen_sessions.remove(timerMsg.session_id);
        //context.getLog().warn( String.format("Session %s of type ? between [%s] and [%s]  : message arrives to early before ? ms", timerMsg.session_id, timerMsg.replyTo.toString(), self.toString()));

    }
    public static void onLBTimer(
        ActorContext context, 
        ActorRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        Set<UUID> dead_sessions, 
        List<Map<UUID, ?>> intermediate_states,
        LBSessionTimer timerMsg){
        context.getLog().debug("receive LBSessionTimer");

        frozen_sessions.remove(timerMsg.session_id);

        timerMsg.replyTo.tell(new SessionIsFrozen(timerMsg.session_id, (ActorRef) self));
    } 
    public static void onIsFrozen(
        ActorContext context, 
        ActorRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        Set<UUID> dead_sessions, 
        List<Map<UUID, ?>> intermediate_states, 
        SessionIsFrozen timerMsg){
        context.getLog().debug("receive IsFrozen");

        frozen_sessions.remove(timerMsg.session_id);
        dead_sessions.add(timerMsg.session_id);

        timerMsg.replyTo.tell(new SessionIsDead(timerMsg.session_id, (ActorRef) self));
    }

    //Ack
    public static void onSessionIsDead(
        ActorContext context, 
        ActorRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        Set<UUID> dead_sessions, 
        List<Map<UUID, ?>> intermediate_states,
        AckSessionIsDead timerMsg){
        context.getLog().debug("receive SessionIsDead");

        frozen_sessions.remove(timerMsg.session_id);
        timeout_sessions.remove(timerMsg.session_id);
        dead_sessions.remove(timerMsg.session_id);

        for ( Map<UUID, ?> intermediate_state : intermediate_states){
            intermediate_state.remove(timerMsg.session_id);
        }
        timerMsg.replyTo.tell(new AckDeadSession(timerMsg.session_id, (ActorRef) self));
    }
    public static void onAckDeadSession(
        ActorContext context, 
        ActorRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        Set<UUID> dead_sessions, 
        List<Map<UUID, ?>> intermediate_states,
        AckSessionIsDead timerMsg){
        context.getLog().debug("receive AckDeadSession");

        frozen_sessions.remove(timerMsg.session_id);
        dead_sessions.remove(timerMsg.session_id);
        timeout_sessions.remove(timerMsg.session_id);

        for ( Map<UUID, ?> intermediate_state : intermediate_states){
            intermediate_state.remove(timerMsg.session_id);
        }
    }
}