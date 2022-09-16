package com.varda;

import akka.actor.Address;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.Props;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import akka.actor.typed.javadsl.AskPattern;
import akka.cluster.MemberStatus;
import akka.cluster.sharding.typed.ShardingEnvelope;
import akka.cluster.sharding.typed.javadsl.ClusterSharding;
import akka.cluster.sharding.typed.javadsl.Entity;
import akka.cluster.typed.Cluster;
import akka.cluster.typed.ClusterSingleton;
import akka.cluster.typed.Join;
import akka.cluster.typed.SingletonActor;
import akka.actor.typed.javadsl.TimerScheduler;

import io.vavr.*;
import java.util.*;
import com.bmartin.*;

public abstract class AbstractComponent<T> extends AbstractBehavior<T> {
    public static final String NAME = "{{system_name}}_component_";
    HashMap<UUID,  InPort> registered_session = new HashMap();
    String schema = "";
    
    public ActorRef<SpawnProtocol.Command> guardian;

    public AbstractComponent(ActorContext<T> context){
        super(context);
        
        // Cluster
        Cluster cluster = Cluster.get(context.getSystem());
        assert (null != cluster);

        // register to receptionist to allow reflexivity : componentsat place -> actoref list
        PlaceDiscovery.register(
            context, 
            this.getClass(),
            cluster.selfMember().address(),
            new ActivationRef(this.schema, context.getSelf(), false, SerializableOptional.empty())
            );
    }

    // (bridge_id, port.expecting_st) -> port_id
    private HashMap<Tuple2<UUID, ASTStype.Base>, UUID> currently_inport_bindings = new HashMap();

    public Void bind_in(InPort port, Bridge bridge, ActivationRef current) {
        // Check that there is at most once (bridge.id, port.expecting_st) couple defined at a time
        // See Wiki for more details

        Tuple2<UUID, ASTStype.Base> key = Tuple.of(bridge.id, port.expecting_st);

        if (this.currently_inport_bindings.containsKey(key)) {
            // Do not crash if it is the same port that is rebound
            UUID port_id = this.currently_inport_bindings.get(key);
            if (port_id != port.id)
                throw new RuntimeException("a port with the same expecting_st is currently binded with the same bridge !!");
        }

        // Perform the valid binding
        port.bind(bridge);

        // Register the valid binding
        this.currently_inport_bindings.put(key, port.id);

        //Register the activations 
        if(bridge.protocol.get_st().equals(port.expecting_st) || bridge.protocol.get_st().equals(port.expecting_st.dual()) )
            bridge.rightRegister(getContext(), this.guardian, current);

        return null;
    }

    public Void bind_in(InPort port, Bridge bridge) {
        return this.bind_in(port, bridge, this.activation_ref());
    }

    public ActivationRef activation_ref(){
        return new ActivationRef(this.schema, getContext().getSelf(), false, SerializableOptional.empty());
    }

    public Void bind_out(OutPort port, Bridge bridge, ActivationRef current) {
        //Register the activations 
        if(bridge.protocol.get_st().equals(port.expecting_st) || bridge.protocol.get_st().equals(port.expecting_st.dual()) )
            bridge.leftRegister(getContext(), this.guardian, current);

        return port.bind(bridge);
    }

    public Void bind_out(OutPort port, Bridge bridge) {
        return this.bind_out(port, bridge, this.activation_ref());
    }

    public List<InPort> reflexivity_inports() {
        return List.of();
    }
    
    public void print_inports(){
        String str = "Inports :\n";
        for (InPort p : this.reflexivity_inports()){
            str += "\t-"+p.toString()+"\n";
        }
        str += "\n";
        getContext().getLog().debug(str);
    }

    public InPort get_intermediate_port(Session s, SerializableOptional<String> receive_id){
        //TODO could optimize by building indexes
        for (InPort p : this.reflexivity_inports()){
            if(p.is_intermediate && p.expecting_st.equals(s.st) && p.receive_id.equals(receive_id) && s.bridge_id.equals(p.bridge.id)){
                return p; 
            }
        }
        throw new RuntimeException("Intermediate port not found at "+receive_id);
    }

    public void print_registered_sessions(){
        String str = "Registered sessions :\n";
        for (Map.Entry<UUID,InPort> entry : this.registered_session.entrySet()){
            str += "\t-"+entry.getKey().toString()+"\n";
        }
        str += "\n";
        getContext().getLog().debug(str);
    }
}
