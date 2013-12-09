package org.lwes;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;

public class TestRunner {

    public TestRunner() {

    }

    public static void main(String[] args) throws Exception{
        int testInterval = Integer.parseInt(args[0]);
        int listenPort = Integer.parseInt(args[1]);
        Map<String, Integer> langPorts = langPorts(args[2]);
        String jsonFile = args[3];
        
        Event source = eventFromJsonFile(jsonFile);
        
        LwesListener listener = new LwesListener(listenPort, langPorts, source);
        List<LwesEmitter> emitters = startEmitters(jsonFile, langPorts);
        Thread.sleep(testInterval * 1000);
        stop(emitters);
        
        Map<String, Boolean> results = listener.getResults();
        if(validateResults(langPorts, results))
            System.exit(1);
        
        System.exit(0);
    }
    
    public static boolean validateResults(Map<String, Integer> langPorts, Map<String, Boolean> langResults){
        boolean allSuccess = true;
        for(String lang:langPorts.keySet()){
            if(langResults.containsKey(lang)){
                boolean result = langResults.get(lang);
                if(result)
                    System.out.println("Successfully decoded the event from " + lang + " to java ");
                else{
                    System.out.println("Failed to correctly decode the event from " + lang + " to java ");
                    allSuccess = false;
                }
            }
            else{
                allSuccess = false;
                System.out.println("Never recieved the event originating from " + lang + " to java ");
            }
        }
        return allSuccess;
    }
    
    public static void stop(List<LwesEmitter> emitters){
        for(LwesEmitter em: emitters)
            em.stop();
    }
    
    public static List<LwesEmitter> startEmitters(String jsonFile, Map<String, Integer> langPorts) throws IOException{
        List<LwesEmitter> emitters = new ArrayList<LwesEmitter>();
        Event e = eventFromJsonFile(jsonFile);    
        e.setString("language", "java");
        for(Integer port : langPorts.values()){
            LwesEmitter emitter = LwesEmitter.emitterFor(e, port);
            new Thread(emitter).start();
        }
        return emitters;
    }
    

    public static Map<String, Integer> langPorts(String emitterOption){
        
        Map<String, Integer> langPortEmitters = new HashMap<String, Integer>();
        
        if(StringUtils.isNotEmpty(emitterOption)){
            String[] langPorts = StringUtils.split(emitterOption, ',');
            for(String langPort : langPorts){
                String[] langPortPair = StringUtils.split(langPort, ':');
                if(langPortPair.length == 2)
                    langPortEmitters.put(langPortPair[0], Integer.parseInt(langPortPair[1]));
            }
        }
        return langPortEmitters;
    }
    
    public static String optionValue(String[] args, String option){
        for(int i=0;i<args.length -1; i++)
            if(args[i].equals(option))
                return args[i+1];
        
        return StringUtils.EMPTY;
    }
    
    public void handleEvent(Event event) {
        final String eventName = event.getEventName();
        System.out.println(eventName);
    }

    public static Event eventFromJsonFile(String file) throws IOException{
        String json = readFromFile(file);
        return new EventFactory().createEventFromJson(json,
                EventImplementation.MAP_EVENT);
    }

    private static String readFromFile(String jsonFile) throws IOException {
        return FileUtils.readFileToString(new File(jsonFile));
    }
}
