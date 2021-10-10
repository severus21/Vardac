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
    private static void onTimeout(
        ActorContext<?> context, 
        ActorRef<?> self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        List<Map<UUID, ?>> intermediate_states,
        SessionTimer timerMsg) {
        timeout_sessions.add(timerMsg.session_id);
        for ( Map<UUID, ?> intermediate_state : intermediate_states){
            intermediate_state.remove(timerMsg.session_id);
        }
        context.getLog().warn( String.format("Session %s of type ? between [%s] and [%s] has timeout after ? ms", timerMsg.session_id, timerMsg.replyTo.toString(), self.toString()));

    }
    public static void onHBTimer(
        ActorContext<?> context, 
        ActorRef<?> self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        List<Map<UUID, ?>> intermediate_states, 
        HBSessionTimer timerMsg){
        onTimeout(context, self, frozen_sessions, timeout_sessions, intermediate_states, timerMsg);
        timerMsg.replyTo.tell(new SessionHasTimeout(timerMsg.session_id, (ActorRef) self));
    } 
    public static void onHasTimeout(
        ActorContext<?> context, 
        ActorRef<?> self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        List<Map<UUID, ?>> intermediate_states, 
        SessionHasTimeout timerMsg){
        onTimeout(context, self, frozen_sessions, timeout_sessions, intermediate_states, timerMsg);
        timerMsg.replyTo.tell(new AckSessionIsDead(timerMsg.session_id, (ActorRef) self));
    }

    // LB
    private static void onFrozen(
        ActorContext<?> context, 
        ActorRef<?> self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        List<Map<UUID, ?>> intermediate_states, 
        SessionTimer timerMsg) {
        frozen_sessions.add(timerMsg.session_id);
        for ( Map<UUID, ?> intermediate_state : intermediate_states){
            intermediate_state.remove(timerMsg.session_id);
        }
        context.getLog().warn( String.format("Session %s of type ? between [%s] and [%s]  : message arrives to early before ? ms", timerMsg.session_id, timerMsg.replyTo.toString(), self.toString()));

    }
    public static void onLBTimer(
        ActorContext<?> context, 
        ActorRef<?> self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        List<Map<UUID, ?>> intermediate_states,
        LBSessionTimer timerMsg){
        onFrozen(context, self, frozen_sessions, timeout_sessions, intermediate_states, timerMsg);
        timerMsg.replyTo.tell(new SessionIsFrozen(timerMsg.session_id, (ActorRef) self));
    } 
    public static void onIsFrozen(
        ActorContext<?> context, 
        ActorRef<?> self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        List<Map<UUID, ?>> intermediate_states, 
        SessionIsFrozen timerMsg){
        onFrozen(context, self, frozen_sessions, timeout_sessions, intermediate_states, timerMsg);
        timerMsg.replyTo.tell(new AckSessionIsDead(timerMsg.session_id, (ActorRef) self));
    }

    //Ack
    public static void onAckSessionIsDead(
        ActorContext<?> context, 
        ActorRef<?> self,
        Set<UUID> frozen_sessions,
        Set<UUID> timeout_sessions, 
        List<Map<UUID, ?>> intermediate_states,
        AckSessionIsDead timerMsg){
        if (timeout_sessions.contains(timerMsg.session_id))
            timeout_sessions.remove(timerMsg.session_id);
        if (frozen_sessions.contains(timerMsg.session_id))
            frozen_sessions.remove(timerMsg.session_id);
    }
}