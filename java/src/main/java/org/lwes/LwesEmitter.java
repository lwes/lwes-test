package org.lwes;

import java.io.IOException;
import java.net.InetAddress;

import org.lwes.emitter.MulticastEventEmitter;

public class LwesEmitter implements Runnable{

    private Event e;
    private String lang;
    private int port;
    MulticastEventEmitter emitter;
    boolean keepRunning;
    
    public static LwesEmitter emitterFor(Event e, String lang, int port) throws IOException{
        return new LwesEmitter(e, lang, port);
    }
    
    private LwesEmitter(Event e, String lang, int port) throws IOException{
        this.e = e;
        this.lang = lang;
        this.port = port;
        emitter= emitter("127.0.0.1", port);
        keepRunning = true;
    }

    public void run() {
        while(keepRunning){
            try {
                emitter.emit(e);
                Thread.sleep(2000);
            } catch (EventSystemException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            } catch (InterruptedException e1) {
                e1.printStackTrace();
            }
        }
    }
    
    public void stop(){
        keepRunning = false;
    }
    
    public MulticastEventEmitter emitter(String host, int port) throws IOException   {
        MulticastEventEmitter emitter = new MulticastEventEmitter();
        emitter.setMulticastAddress(InetAddress.getByName(host));
        emitter.setMulticastPort(port);
        emitter.initialize();
        
        return emitter;
    }
    
}
