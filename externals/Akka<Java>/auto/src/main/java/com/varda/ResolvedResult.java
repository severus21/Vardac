package com.varda;

import java.util.function.Function;

import java.time.Duration;
import java.util.UUID;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.As;
import com.fasterxml.jackson.annotation.JsonTypeName;


import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;

import io.vavr.*;
import io.vavr.control.*;

public class ResolvedResult extends BaseEvent {
    UUID id;
    Either<Error, Object> value;

    public ResolvedResult(UUID id, Object value){
        assert(id != null);
        assert(value != null);

        this.id = id;
        this.value = Either.right(value);
    }

    @JsonCreator
    public ResolvedResult(@JsonProperty("id") UUID id, @JsonProperty("value") Either<Error, Object> value){
        assert(id != null);
        assert(value != null);

        this.id = id;
        this.value = value;
    }

    public Either<Error, Object> getValue(){
        return this.value;
    }

    public static void onResolvedResult(ActorContext context, Map<UUID, Function<Either<Error, Object>, Either<Error, Void>>> intermediate_futures, ResolvedResult rres){
        assert(context != null);
        assert(intermediate_futures != null);
        assert(rres != null);
        assert(rres.id != null);

        if(rres.value.isLeft()){
            throw new RuntimeException(rres.value.getLeft());
        } else if (intermediate_futures.containsKey(rres.id)){
            Either<Error, Void> res = intermediate_futures.get(rres.id).apply(rres.value);
            if(res.isLeft())
                throw new RuntimeException(res.getLeft());
        } else {
            throw new RuntimeException(rres.id+"is not registered in intermediate_futures\n:"+intermediate_futures);
        }
    }
    
}
