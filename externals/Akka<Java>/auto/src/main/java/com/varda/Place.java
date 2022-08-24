package com.varda;

import scala.collection.JavaConverters;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.varda.VPlaces;

import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Collectors;
import java.util.function.Predicate;
import java.util.*;

import akka.actor.Address;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.ActorRef;
import akka.cluster.Member;
import akka.cluster.MemberStatus;
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

    public String toString(){
        if (this.vp != null && this.address != null)
            return String.format("Place{vp=%s; address=%s}", this.vp.toString(), this.address.toString());
        if (this.vp != null)
            return String.format("Place{vp=%s}", this.vp.toString());
        if (this.address != null)
            return String.format("Place{address=%s}", this.address.toString());
        return "Place{}";
    }

    public static Place of_member(Member member){
        Set<String> roles = JavaConverters.asJava(member.roles());

        //System.out.println(String.format("current roles %d", roles.size()));
        //for(String role : roles){
        //    System.out.println(String.format("\t%s", role));
        //}

        for(String role : roles){
            if (role.startsWith("vp__")){
                return new Place(VPlaces.get(role.substring(4)), member.address());
            }
        }

        return new Place(null, member.address());

    }

    public static Place of_actor_ref(ActorContext context, ActivationRef ref){
        return of_actor_ref(context, ref.actorRef);
    }

    public static Place of_actor_ref(ActorContext context, ActorRef ref){
        assert(ref!=null);
        Address addr = ref.path().address();
        System.out.println("######## "+addr.getHost());
        if( !addr.getHost().isPresent() ){
            return currentPlace(context);
        }

        //Searching
        Iterable<Member> members = Cluster.get(context.getSystem()).state().getMembers();

        for(Member member : members){
            System.out.println(member.address().toString());
            if(member.address().equals(addr))
                return of_member(member); 
        }
        System.out.println("########");
        /// TODO handle error
        return null;
    }

    public static List<Place> places(ActorContext context){ 
        Iterable<Member> members = Cluster.get(context.getSystem()).state().getMembers();
        List<Place> places = StreamSupport.stream(
                members.spliterator(), false)
                .filter(member -> member.status() == MemberStatus.up())
                .map(x -> Place.of_member(x))
                .collect(Collectors.toList());
        assert(places.size() > 0);
        return places;
    }

    public static List<Place> places(ActorContext context, VPlace vp, Predicate<Place> predicate) {
        assert(vp.name != null);
        assert(predicate != null);
        return places(context).stream()
                    .filter(x -> x.vp.equals(vp) && predicate.test(x))
                    .collect(Collectors.toList());
    }

    public static Place currentPlace(ActorContext context){
        return Place.of_member(Cluster.get(context.getSystem()).selfMember());
    }

    public boolean equals(Object obj) {
        if (obj == null)
            return false;

        if (obj == this) {
            return true;
        }

        if (!(obj instanceof Place)) {
            return false;
        }   

        Place b = (Place) obj;
        return this.address.equals(b.address);
    }
}
