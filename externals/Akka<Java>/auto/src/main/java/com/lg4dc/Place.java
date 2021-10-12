package com.lg4dc;

import scala.collection.JavaConverters;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Collectors;
import java.util.function.Predicate;
import java.util.*;

import akka.cluster.Member;
import akka.cluster.MemberStatus;
import akka.actor.Address;
import akka.actor.typed.javadsl.ActorContext;
import akka.cluster.typed.Cluster;

public class Place {
    VPlace vp;
    Address address;


    public Place(VPlace vp, Address address){
        this.vp = vp;
        this.address = address;
    }

    public String getHost() {
        return address.getHost().get(); 
    }

    public int getPort() {
        return address.getPort().get(); 
    }

    public static Place of_member(Member member){
        Set<String> roles = JavaConverters.asJava(member.roles());
        List<VPlace> vps = roles.stream()
            .filter(role -> true)
            .map(name -> VPlaces.vplaces.get(name))
            .collect(Collectors.toList());
        assert(vps.size() == 1);

        return new Place(vps.get(0), member.address());
    }

    public static List<Place> places(ActorContext context){ 
        Iterable<Member> members = Cluster.get(context.getSystem()).state().getMembers();
        return StreamSupport.stream(
                members.spliterator(), false)
                .filter(member -> member.status() == MemberStatus.up())
                .map(x -> Place.of_member(x))
                .collect(Collectors.toList());
    }

    public static List<Place> places(ActorContext context, VPlace vp, Predicate<Place> predicate) {
        return places(context).stream()
                    .filter(x -> x.vp == vp && predicate.test(x))
                    .collect(Collectors.toList());
    }

    public static Place currentPlace(ActorContext context){
        return Place.of_member(Cluster.get(context.getSystem()).selfMember());
    }
}
