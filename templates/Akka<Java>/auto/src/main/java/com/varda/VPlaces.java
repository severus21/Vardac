package com.varda;

import com.google.common.collect.ImmutableMap;

public class VPlaces {

    public static ImmutableMap<String, VPlace> vplaces = new ImmutableMap.Builder<String, VPlace>() {% for (key, vp) in vplaces %}
    .put("{{key}}", new VPlace("{{vp.name}}")){% endfor %}
    .build();

    public static VPlace get(String key){
        //System.out.println("Building vplace from key "+key);
        VPlace tmp = vplaces.get(key);
        assert(tmp != null); //TODO instead raise an exception saying this vp does not exists
        return tmp;
    }

    public static boolean containsKey(String key){
        return vplaces.containsKey(key);
    }
}
