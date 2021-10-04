package com.lg4dc;
import java.util.UUID;
import java.util.List;
import java.util.ArrayList;

import io.vavr.*;

import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;

import com.bmartin.*;

public abstract interface Protocol {
    public abstract ASTStype.Base get_st();
}
/*
//https://apocalisp.wordpress.com/2008/10/23/heterogeneous-lists-and-the-limits-of-the-java-type-system/
public abstract class HList<A extends HList<A>> {
    private HList() {}

    private static final HNil nil = new HNil();

    public static HNil nil() {
        return nil;
    }

    public static <E, L extends HList<L>> HCons<E, L> cons(final E e, final L l) {
        return new HCons<E, L>(e, l);
    }

    public static final class HNil extends HList<HNil> {
        private HNil() {}
    }

    public static final class HCons<E, L extends HList<L>> extends HList<HCons<E, L>> {
        private E e;
        private L l;

        private HCons(final E e, final L l) {
            this.e = e;
            this.l = l;
        }

        public E head() {
            return e;
        }

        public L tail() {
            return l;
        }
    }
}
//Usage : HCons<String, HCons<Integer, HCons<Boolean, HNil>>> x = cons("One", cons(2, cons(false, nil()));


public class STEntry<ST> extends SType<Label, ST> {} 

public class STBranch<Entries extends  HList<Entries>> extends SType<Label, Entries> {} 

public class STSelect <Entries extends  HList<Entries>> extends SType<Label, Entries> {}

public class STRec <ST> extends SType<void, ST> {
    public String name;

    public SType(String name) {
        this.name = name;
    }

    public boolean equals(SType<T, ST> obj) {
        return this.getClass() == obj.getClass() && this.name == obj.name 
    }
}

public interface Command extends CborSerializable {
}

public class PCommand<Msg extends Command> extends Command {
    Msg value;
    int session_id;
    String bridge;
}

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ProtocolTest extends AllDirectives {
    @BeforeEach
    public void setUp() {}

    @AfterEach
    public void tearDown() {}
    
    @Test
    public void equalityTest() {
        assertEquals();
    }
}

class Protocol<Msg> {
    String bridge_id;
    int session_id;
    ActorRef<component type> other;


    public interface Command extends CborSerializable{}

    public class Event<Msg extends Command> extends Command {
        public Event(int session_id, String brdige_id, Msg msg){
            this.session_id = session_id;
            this.bridge_id = bridge_id;
            this.msg = msg;
        }

        Msg msg;
        int session_id;
        String bridge_msg;
    }

    public static final class Ping implements Event {}
    public static final class Pong implements Event {}

    // timeout 1 is !ping timeout ?pong
    // For timeout 1 in protocol
    private static Object TICK_KEY_1 = "TickKey1";

    private static final class Tick1 {}

    //Timout n
    //...

    public void fire_ping_1(Ping msg) {
        other.tell(msg);
        
        //start timeout if needed
        getTimers().startSingleTimer(TICK_KEY_1, new Tick1(), Duration.ofMillis(delay1));
    }

    //dual fct
    public void async_receive_ping_1(Ping msg) {
        .onMessage( => port
            Ping.class,
            replyTo -> {
                assert(replyTo == other);
                return Behaviors.same(); //can not change behavior since we can have multiple session in parallel
            })
        .onMessage( 
            TickKey1.class,
            message -> cancelTimer(TICK_KEY_1) //only one supported yet
        )
    } 

    public void receive_ping_1(Print msg) {
        getTimers().startSingleTimer(TICK_KEY_1, new Tick1(), Duration.ofMillis(delay1));
    }
    
    public void receive_pong_2(Print msg) {
        cancelTimer(TICK_KEY_1);
    }
    
    public void send_pong_2(Print msg) {
        cancelTimer(TICK_KEY_1);
    }
}
*/