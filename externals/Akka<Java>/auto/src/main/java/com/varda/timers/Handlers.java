package com.varda.timers;

import java.time.Duration;
import java.util.UUID;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import java.util.List;


import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;

import com.bmartin.CborSerializable;
import com.varda.ASTStype;
import com.varda.ActivationRef;
import com.varda.timers.AckDeadSession;
import com.varda.timers.SessionIsDead;


public class Handlers {
    public static void onHBTimer(
        ActorContext context, 
        ActivationRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> dead_sessions, 
        List<HashMap<UUID, ?>> intermediate_states, 
        HBSessionTimer timerMsg
    ){
        context.getLog().debug("receive HBSessionTimer");
        context.getLog().warn( String.format("Session %s of type ? between [%s] and [%s] has timeout after ? ms", timerMsg.session_id, timerMsg.replyTo.toString(), self.toString()));

        dead_sessions.add(timerMsg.session_id);

        timerMsg.replyTo.actorRef.tell(new SessionIsDead(timerMsg.session_id, self));
    } 
    public static void onLBTimer(
        ActorContext context, 
        ActivationRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> dead_sessions, 
        List<HashMap<UUID, ?>> intermediate_states,
        LBSessionTimer timerMsg
    ){
        context.getLog().debug("receive LBSessionTimer");
        frozen_sessions.remove(timerMsg.session_id);
    } 


    //Ack
    public static void onSessionIsDead(
        ActorContext context, 
        ActivationRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> dead_sessions, 
        List<HashMap<UUID, ?>> intermediate_states,
        SessionIsDead timerMsg
    ){
        context.getLog().debug("receive SessionIsDead");

        frozen_sessions.remove(timerMsg.session_id);
        dead_sessions.remove(timerMsg.session_id);

        for ( HashMap<UUID, ?> intermediate_state : intermediate_states){
            intermediate_state.remove(timerMsg.session_id);
        }
        timerMsg.replyTo.actorRef.tell(new AckDeadSession(timerMsg.session_id, self));
    }

    public static void onAckDeadSession(
        ActorContext context, 
        ActivationRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> dead_sessions, 
        List<HashMap<UUID, ?>> intermediate_states,
        AckDeadSession timerMsg
    ){
        context.getLog().debug("receive AckDeadSession");

        frozen_sessions.remove(timerMsg.session_id);
        dead_sessions.remove(timerMsg.session_id);

        for ( HashMap<UUID, ?> intermediate_state : intermediate_states){
            intermediate_state.remove(timerMsg.session_id);
        }
    }

    //
    public static boolean is_session_alive(
        ActorContext context, 
        ActivationRef self,
        Set<UUID> frozen_sessions,
        Set<UUID> dead_sessions, 
        UUID session_id,
        ActivationRef replyTo
    ){

        if(frozen_sessions.contains(session_id)){
            context.getLog().warn( String.format("Session %s of type ? between [%s] and [%s]  : lower bound [? ms] violated", session_id, replyTo.toString(), self.toString()));
            dead_sessions.add(session_id);
            replyTo.actorRef.tell(new SessionIsDead(session_id, self));
            return false;
        } else if (dead_sessions.contains(session_id)){
            replyTo.actorRef.tell(new SessionIsDead(session_id, self));
            return false;
        }

        return true;
    }

    public static void onResolvedResult(ActorContext context, Map<UUID, Function<?, Void>> intermediate_futures, ResolvedResult rres){
        intermediate_futures.get(rres.id).apply(rres.value);
    }
}