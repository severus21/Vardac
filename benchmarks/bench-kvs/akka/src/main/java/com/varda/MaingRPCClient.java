package com.varda;
import akka.grpc.GrpcClientSettings;
import com.typesafe.config.ConfigFactory;
import com.varda.grpc.*;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

class MaingRPCClient {

   
   public akka.actor.ActorSystem system;
   
   
   public KVProtoServiceClient client;
   
   
   public MaingRPCClient( String host,   int port) {
   
      
        this(host,port,akka.actor.ActorSystem.create("MaingRPCClient", 
            ConfigFactory.parseString(
                "akka.cluster.jmx.multi-mbeans-in-same-jvm=on \n" +
                "akka.remote.artery.canonical.hostname=127.0.0.1\n" +
                "akka.remote.artery.canonical.port=0 \n" +
                "akka.cluster.roles=[\"client\"] \n" +
                "akka.cluster.seed-nodes=[\"akka://@" + host + ":" + port + "\"] \n")));
                            
   }
   
   public MaingRPCClient(String host, int port, akka.actor.ActorSystem system) {
      
      this.system = system;
                            
      GrpcClientSettings settings = GrpcClientSettings.connectToServiceAt(host, port, this.system).withTls(false);

      try{this.client = 
          KVProtoServiceClient.create(settings,  this.system);
      }catch (Exception e){
         System.out.println(e);
      }
   }
   
   Boolean put( String k,   String v) throws InterruptedException, 
   ExecutionException, 
   TimeoutException {
   
      ProtoPutRequest request = ProtoPutRequest.newBuilder().setKey(k).setValue(v).build();
      CompletionStage<ProtoPutResponse> reply = 
      this.client.put(request);
      return reply.toCompletableFuture().get(5, TimeUnit.SECONDS).getFlag();
   }
   
   Boolean delete(String k) throws InterruptedException, 
   ExecutionException, 
   TimeoutException {
   
      ProtoDeleteRequest request = ProtoDeleteRequest.newBuilder().setKey(k).build();
      CompletionStage<ProtoDeleteResponse> reply = 
      this.client.delete(request);
      return reply.toCompletableFuture().get(5, TimeUnit.SECONDS).getFlag();
   }
   
   String get(String key) throws InterruptedException, 
   ExecutionException, 
   TimeoutException {
      ProtoGetRequest request = ProtoGetRequest.newBuilder().setKey(key).build();
      CompletionStage<ProtoGetResponse> reply = 
         this.client.get(request);
      return reply.toCompletableFuture().get(5, TimeUnit.SECONDS).getValue();
   }
   
    public void disconnect() {
        this.system.terminate();
    }
                                
}
