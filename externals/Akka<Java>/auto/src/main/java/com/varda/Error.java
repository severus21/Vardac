package com.varda;

public class Error extends RuntimeException {
    String msg;

    public Error(String msg){
        super(msg);
        this.msg = msg;
    }
    
}
