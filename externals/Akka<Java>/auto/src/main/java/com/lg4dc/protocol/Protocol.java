package com.lg4dc.protocol;
import java.util.UUID;
import java.util.List;

import io.vavr.*;

import akka.actor.typed.ActorRef;

public class Protocol {
    // Session type encoded as type parameter + empty values 
    public static final class SType {
        public abstract class SType<T, ST> {
            public T value = null;
            public ST continuation = null;

            @Override
            public boolean equals(Object obj) {
            return false; 
            }

            //FIXME il faudrait le paramètre obj est exacte ou utiliser getClass
            public boolean equals(SType<T, ST> obj) {
                return this.getClass() == obj.getClass() && this.value.equals(obj.value) && this.continuation.equals(obj.continuation); 
            }
        }

        public static final class STLabel {
            public String name;
        }

        public static final class STEnd extends SType<Void, Void> {}

        public static final class STRecv<T, ST> extends SType<T, ST> {
            public STRecv(T val, ST cont) {
                this.value = val;
                this.continuation = cont;
            }
        }

        public static final class STSend<T, ST> extends SType<T, ST> {
            public STSend(T val, ST cont) {
                this.value = val;
                this.continuation = cont;
            }
        }

        public static final class STVar extends SType<Void, Void> {
            public String name;

            public STVar(String name) {
                this.name = name;
            }

            public boolean equals(STVar obj) {
                return this.getClass() == obj.getClass() && this.name == obj.name;
            }
        } 
    }

    // Session types as values
    public static final class ASTStype {
        public static final class MsgT {
            public Object value;
            public MsgT(Object value){
                this.value = value;
            }
        }
        public static class Base {
            /*
                Empty continuation <=> End
                Send and Receive [("", continuation)]
                Choice and Select [ (label, continuaiton_label); ....]
                Recursive ????? TODO
            */
            public List<Tuple2<MsgT, Base>> continuations = new List<>();

        }


        public static final class End extends Base {
            public End () {
                assert(this.continuations.isEmpty());
            }
        }

        public static final class Send extends Base {
            public MsgT msg_type;
            public Send(MsgT msg_type, Base continuation){
                this.msg_type = msg_type;
                this.continuations.add(new Tuple2(msg_type, continuation));

                assert(!this.continuations.isEmpty());
            }

        }
        public static final class Receive extends Base {
            public MsgT msg_type;
            public Receive(MsgT msg_type, Base continuation){
                this.msg_type = msg_type;
                this.continuations.add(new Tuple2(msg_type, continuation));

                assert(!this.continuations.isEmpty());
            }

        }
        public static final class Branch extends Base {
            public Branch(List<Tuple2<MsgT, Base>> continuations){
                this.continuations = continuations;

                assert(!this.continuations.isEmpty());
            }
        }
        public static final class Select extends Base {
            public Select(List<Tuple2<MsgT, Base>> continuations){
                this.continuations = continuations;

                assert(this.continuations.isEmpty());
            }
        }
        // TODO recursion
    }

    public static final class PProtocol {
        ASTStype.Base st; //Set inside constructor subclass

        public PProtocol () {}
    }

    public static abstract class AbstractSession {

    }

    public static final class Bridge<P extends PProtocol> {
        String id;
        P protocol;
        
        protected int session_counter = 0;

        public Bridge (P p) {
            this.id = UUID.randomUUID().toString();
            this.protocol = p;
        }

        public String get_id(){
            return this.id;
        }

        public AbstractSession init_session_with(ActorRef<?> right){
            this.session_counter += 1;

            return P.new Session(this.id, right, this.session_counter, this.protocol.st);
        }    
    }

    public static final class Session<Msg> {
        public class Event<Msg extends CborSerializable> implements CborSerializable {
            String bridge_id;
            int session_id;
            ASTStype.Base st;
            Msg msg;
            
            public Event(ActorContext<Command> context, 
                            String bridge_id, 
                            int session_id, 
                            ASTStype.Base st, 
                            Msg msg) {
            
               super(context);
               this.bridge_id = bridge_id;
               this.session_id = session_id;
               this.st = st;
               this.msg = msg;
            }
        }
 
        String bridge_id;
        ActorRef<?> right;
        int session_id;
        ASTStype.Base st;
        
        public Session fire(Msg msg,  ActorContext context) {
           assert(self.st.continuations.size() == 1);
           
           Event<Msg> e = Event(this.bridge_id,  this.session_id,  this.st,  msg);
           this.right.tell(e, context.getSelf());

           this.st = this.st.continuations.get(1)._2;
           return this;
        }
        
        /*public Tuple2<Object, Session> receive(ActorContext context) {
           assert(self.st.continuations.size() == 1);
        
           Duration timeout = Duration.ofSeconds(3);
           CompletableFuture<Event<Object>> future = 
           ask(this.right,  msg,  timeout).
           toCompletableFuture();
           Event<Object> e = future.join(3);

           this.st = this.st.continuations.get(1)._2;
           return new Tuple2(e.msg,  this);
        }*/
    }
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