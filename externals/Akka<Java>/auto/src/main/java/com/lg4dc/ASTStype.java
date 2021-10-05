package com.lg4dc;
import java.util.List;
import java.util.ArrayList;

import io.vavr.*;
// Session types as values
public final class ASTStype {
    public static final class MsgT {
        public Object value;
        public MsgT(Object value){
            this.value = value;
        }

        public boolean equals(Object obj) {
            System.out.println(">>>> equals MsgT");
            if (obj == this) {
                return true;
            }

            if (!(obj instanceof MsgT)) {
                return false;
            }   

            MsgT b = (MsgT) obj;
            System.out.printf(">>>> equals MsgT %b %s %s", this.value.equals(b.value), this.value.toString(), b.value.toString());
            return this.value.equals(b.value);
        }

    }
    public static class Base {
        /*
            Empty continuation <=> End
            Send and Receive [("", continuation)]
            Choice and Select [ (label, continuaiton_label); ....]
            Recursive ????? TODO
        */
        public List<Tuple2<MsgT, Base>> continuations = new ArrayList<>();

        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }

            if (!(obj instanceof Base)) {
                return false;
            }   

            Base b = (Base) obj;
            return this.continuations.equals(b.continuations); 
        }

    }


    public static final class End extends Base {
        public End () {
            assert(this.continuations.isEmpty());
        }

        public boolean equals(End obj) {
            System.out.println(">>>> equals End");
            assert(false);
            return true; 
        }
    }

    public static final class Send extends Base {
        public MsgT msg_type;
        public Send(MsgT msg_type, Base continuation){
            this.msg_type = msg_type;
            this.continuations.add(new Tuple2(msg_type, continuation));

            assert(!this.continuations.isEmpty());
        }

        public boolean equals(Send obj) {
            System.out.println(">>>> equals send");
            assert(false);
            return this.msg_type.equals(obj.msg_type) && this.continuations.equals(obj.continuations); 
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