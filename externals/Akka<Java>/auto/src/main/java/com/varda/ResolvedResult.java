package com.varda;

import java.util.function.Function;

import java.time.Duration;
import java.util.UUID;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import java.util.List;

import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;

import io.vavr.*;
import io.vavr.control.*;

public class ResolvedResult extends BaseEvent {
    UUID id;
    Either<Error, Object> value;

    public ResolvedResult(UUID id, Object value){
        this.id = id;
        this.value = Either.right(value);
    }

    public ResolvedResult(UUID id, Either<Error, Object> value){
        this.id = id;
        this.value = value;
    }

    public static void onResolvedResult(ActorContext context, Map<UUID, Function<Either<Error, Object>, Either<Error, Void>>> intermediate_futures, ResolvedResult rres){
        if(rres.value.isLeft()){
            throw new RuntimeException(rres.value.getLeft());
        } else {
            Either<Error, Void> res = intermediate_futures.get(rres.id).apply(rres.value);
            if(res.isLeft())
                throw new RuntimeException(res.getLeft());
        }
    }
    
}
