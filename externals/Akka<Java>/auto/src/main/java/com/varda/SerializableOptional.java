package com.varda;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import com.bmartin.*;

public class SerializableOptional<T> implements CborSerializable, JsonSerializable, java.io.Serializable{
    @JsonProperty("is_empty")    
    public final Boolean is_empty;
    
    @JsonProperty("value")
    public final T value;
 
    @JsonCreator
    private SerializableOptional(T value, Boolean is_empty) {
        this.value = value;
        this.is_empty = is_empty; 
    }
  
    public static SerializableOptional empty() {
        return new SerializableOptional(null, true);
    }
  
    private SerializableOptional(T value) {
        this.value = value;
        this.is_empty = false;
    }
  
    public static <T> SerializableOptional of(T value) {
        return new SerializableOptional(value);
    }
  
    public  T get() {
        if(!this.is_empty)
            return this.value;

        throw new RuntimeException("empty SerializableOptional");
    }
  
    public boolean isPresent() {
        return !this.is_empty;
    }
    
    public boolean isEmpty() {
        return this.is_empty;
    }
    
    @Override
    public boolean equals(java.lang.Object obj) {
        if(obj == null)
            return false;
        SerializableOptional<T> o = (SerializableOptional<T>) obj;
        return o.is_empty.equals(this.is_empty) && this.value.equals(o.value);
    }
    
    @Override
    public java.lang.String toString() {
        if(!this.is_empty)
            return "SerializableOptional<"+this.value+">";
        else
            return "SerializableOptional<Empty>";
    }
}