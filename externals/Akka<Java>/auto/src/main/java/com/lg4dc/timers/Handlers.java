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
    private static void onTimeout(ActorContext<?> context, ActorRef<?> self, Set<UUID> timeout_sessions, List<Map<UUID, ?>> intemeditate_states, SessionTimer timerMsg) {
        timeout_sessions.add(timerMsg.session_id);
        for ( Map<UUID, ?> intermediate_state : intermediate_states){
            intermediate_state.remove(e.session_id);
        }
        context.getLog().warn( String.format("Session %s of type ? between [%s] and [%s] has timeout after ? ms", timerMsg.session_id, ActorRef.to_string(timerMsg.replyTo), ActorRef.to_string(self)));

    }
    public static void onHBTimer(ActorContext<?> context, 
    ActorRef<?> self,
    Set<UUID> timeout_sessions, List<Map<UUID, ?>> intemeditate_states, HBSessionTimer timerMsg){
        onTimeout(context, self, timeout_sessions, intemeditate_states, timerMsg);
        timerMsg.replyTo.tell(new SessionHasTimeout(timerMsg.session_id, self));
    } 
    public static void onHasTimeout(ActorContext<?> context, ActorRef<?> self, Set<UUID> timeout_sessions, List<Map<UUID, ?>> intemeditate_states, SessionHasTimeout timerMsg){
        onTimeout(context, self, timeout_sessions, intemeditate_states, timerMsg);
        timerMsg.replyTo.tell(new AckSessionIsDead(timerMsg.session_id, self));
    }

    // LB
    private static void onFrozen(ActorContext<?> context, ActorRef<?> self, Set<UUID> frozen_sessions, List<Map<UUID, ?>> intemeditate_states, SessionTimer timerMsg) {
        frozen_sessions.add(timerMsg.session_id);
        for ( Map<UUID, ?> intermediate_state : intermediate_states){
            intermediate_state.remove(e.session_id);
        }
        context.getLog().warn( String.format("Session %s of type ? between [%s] and [%s]  : message arrives to early before ? ms", timerMsg.session_id, ActorRef.to_string(timerMsg.replyTo), ActorRef.to_string(self)));

    }
    public static void onLBTimer(ActorContext<?> context, 
    ActorRef<?> self,
    Set<UUID> frozen_sessions, List<Map<UUID, ?>> intemeditate_states, LBSessionTimer timerMsg){
        onFrozen(context, self, frozen_sessions, intemeditate_states, timerMsg);
        timerMsg.replyTo.tell(new SessionIsFrozen(timerMsg.session_id, self));
    } 
    public static void onIsFrozen(ActorContext<?> context, ActorRef<?> self, Set<UUID> frozen_sessions, List<Map<UUID, ?>> intemeditate_states, SessionIsFrozen timerMsg){
        onFrozen(context, self, frozen_sessions, intemeditate_states, timerMsg);
        timerMsg.replyTo.tell(new AckSessionIsDead(timerMsg.session_id, self));
    }

    //Ack
    public static void onAckSessionIsDead(ActorContext<?> context, Set<UUID> frozen_sessions, Set<UUID> timeout_sessions, AckSessionIsDead timerMsg){
        if (timeout_sessions.containsKey(timerMsg.session_id))
            timeout_sessions.remove(timerMsg.session_id);
        if (frozen_sessions.containsKey(timerMsg.session_id))
            frozen_sessions.remove(timerMsg.session_id);
    }
}