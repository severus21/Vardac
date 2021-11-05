package com.lg4dc;

import java.util.List;

public class VPlace {
    //DC::Node -> [DC, Node]
    String name = ""; 

    public VPlace(String name){
        assert(name != null);
        this.name = name;
    }    

    public boolean equals(Object obj) {
        assert(obj != null);
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof VPlace)) {
            return false;
        }   

        VPlace b = (VPlace) obj;
        return this.name.equals(b.name);
    }

    public String toString(){
        return this.name;
    }
}
