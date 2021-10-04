package com.lg4dc;

/*
// Session type encoded as type parameter + empty values 
public final class SType {
    public abstract class BaseSType<T, ST extends BaseSType<?, ?> > {
        public T value = null;
        public ST continuation = null;

        @Override
        public boolean equals(Object obj) {
        return false; 
        }

        //FIXME il faudrait le paramètre obj est exacte ou utiliser getClass
        public boolean equals(BaseSType<T, ST> obj) {
            return this.getClass() == obj.getClass() && this.value.equals(obj.value) && this.continuation.equals(obj.continuation); 
        }
    }

    public static final class STLabel {
        public String name;
    }

    public static final class STEnd extends BaseSType<Void, STEnd> {}

    public static final class STRecv<T, ST extends BaseSType<?, ?> > extends BaseSType<T, ST> {
        public STRecv(T val, ST cont) {
            this.value = val;
            this.continuation = cont;
        }
    }

    public static final class STSend<T, ST extends BaseSType<?, ?> > extends BaseSType<T, ST> {
        public STSend(T val, ST cont) {
            this.value = val;
            this.continuation = cont;
        }
    }

    public static final class STVar extends BaseSType<Void, STVar> {
        public String name;

        public STVar(String name) {
            this.name = name;
        }

        public boolean equals(STVar obj) {
            return this.getClass() == obj.getClass() && this.name == obj.name;
        }
    } 
}
*/