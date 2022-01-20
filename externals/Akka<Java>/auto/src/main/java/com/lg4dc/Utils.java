package com.lg4dc;

import java.util.*;

public final class Utils {
    
    static <T> T pick(AbstractMap<T> map){
        List<T> valuesList = new ArrayList<T>(map.values());
        int randomIndex = new Random().nextInt(valuesList.size());
        return valuesList.get(randomIndex);
    }    
}