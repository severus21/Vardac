package com.lg4dc;

import com.google.common.collect.ImmutableMap;

public class VPlaces {

    public static ImmutableMap<String, VPlace> vplaces = new ImmutableMap.Builder<String, VPlace>() {% for (key, vp) in vplaces %}
    .put("{{key}}", new VPlace("{{vp.name}}")){% endfor %}
    .build();

    public static VPlace get(String key){
        //System.out.println("Building vplace from key "+key);
        return vplaces.get(key);
    }

    public static boolean containsKey(String key){
        return vplaces.containsKey(key);
    }
}
