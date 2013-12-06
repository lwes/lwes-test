package org.lwes;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.StringUtils;
import org.lwes.listener.DatagramEventListener;
import org.lwes.listener.EventHandler;

public class LwesListener {

    private Map<String, Boolean> results;
    
    LwesListener(int listenPort, Map<String, Integer> langPorts, Event source) throws UnknownHostException{
        results = new ConcurrentHashMap<String, Boolean>();
        EventHandler handler = new TestEventHandler(results, source);
        InetAddress address = InetAddress.getByName("127.0.0.1");
        DatagramEventListener listener = new DatagramEventListener();
        listener.setAddress(address);
        listener.setPort(listenPort);
        listener.addHandler(handler);
        listener.initialize();
    }
    
    Map<String, Boolean> getResults(){
        return results;
    }
}

class TestEventHandler implements EventHandler{

    Map<String, Boolean> results;
    Event source;
    
    TestEventHandler(Map<String, Boolean> results, Event source){
        this.results = results;
        this.source = source;
    }
    
    public void handleEvent(Event event) {
        String lang = event.getString("language");
        if(StringUtils.isNotEmpty(lang)){ 
            if(validEvent(event))
                results.put(lang, true);
            else
                results.put(lang, false);
        }
        
    }

    private boolean validEvent(Event event) {
        //Compare the events here
        return true;
    }

    public void destroy() {
        
    }
    
}