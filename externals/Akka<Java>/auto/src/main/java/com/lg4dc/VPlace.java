package com.lg4dc;

import java.util.List;

public class VPlace {
    //DC::Node -> [DC, Node]
    String name; 

    public VPlace(String name){
        this.name = name;
    }    

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof VPlace)) {
            return false;
        }   

        VPlace b = (VPlace) obj;
        return this.name.equals(b.name);
    }
}
