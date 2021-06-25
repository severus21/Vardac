//package com.lg4dc.protocol;

class SType<T, ST> {
    public T value;
    public ST continuation;

    public SType(T val, ST cont) {
        this.value = val;
        this.continuation = cont;
    }

    @Override
    public boolean equals(Object obj) {
       return false; 
    }

    //FIXME il faudrait le paramètre obj est exacte ou utiliser getClass
    public boolean equals(SType<T, ST> obj) {
        return this.getClass() == obj.getClass() && this.value.equals(obj.value) && this.continuation.equals(obj.continuation) 
    }
}

class Label {
    public String name;
}

public class End extends SType<void, void> {}

public class Recv<T, ST> extends SType<T, ST> {}

public class Send<T, ST> extends SType<T, ST> {}

public class Var extends SType<void, void> {
    public String name;

    public SType(String name) {
        this.name = name;
    }

    public boolean equals(SType<T, ST> obj) {
        return this.getClass() == obj.getClass() && this.name == obj.name 
    }
} 

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