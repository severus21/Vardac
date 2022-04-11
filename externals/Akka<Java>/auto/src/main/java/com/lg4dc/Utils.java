package com.lg4dc;

import java.util.*;

public final class Utils {
    
    static <K, V> V pick(AbstractMap<K, V> map){
        List<V> valuesList = new ArrayList<V>(map.values());
        int randomIndex = new Random().nextInt(valuesList.size());
        return valuesList.get(randomIndex);
    }    
}