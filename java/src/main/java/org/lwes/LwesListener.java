package org.lwes;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.StringUtils;
import org.lwes.listener.DatagramEventListener;
import org.lwes.listener.EventHandler;
import org.lwes.util.IPAddress;

import com.google.common.collect.ImmutableSet;

import static org.lwes.Helper.*;

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
    Set<String> ignore = ImmutableSet.of("language", "ReceiptTime", "SenderPort", "SenderIP");
    
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
        boolean equal = true;
        Set<String> attributeNames = event.getEventAttributes();
        for(String attributeName:attributeNames){
            if(ignore.contains(attributeName))
                continue;
            FieldType ft = event.getType(attributeName);
            
            switch (ft) {
            case BOOLEAN:
                equal = event.getBoolean(attributeName) == source.getBoolean(attributeName);
                break;
            case BYTE:
                equal = event.getByte(attributeName) == source.getByte(attributeName);
                break;
            case DOUBLE:
                equal = same(event.getDouble(attributeName), source.getDouble(attributeName));
                break;
            case FLOAT:
                equal = same(event.getFloat(attributeName), source.getFloat(attributeName));
                break;
            case INT16:
                equal = same(event.getInt16(attributeName), source.getInt16(attributeName));
                break;
            case INT32:
                equal = same(event.getInt32(attributeName), source.getInt32(attributeName));
                break;
            case INT64:
                equal = same(event.getInt64(attributeName), source.getInt64(attributeName));
                break;
            case IPADDR:
                equal = new IPAddress(event.getIPAddress(attributeName)).equals(new IPAddress(source.getIPAddress(attributeName)));
                break;
            case STRING:
                equal = StringUtils.equals(event.getString(attributeName), source.getString(attributeName));
                break;
            case UINT16:
                equal = same(event.getUInt16(attributeName), source.getUInt16(attributeName));
                break;
            case UINT32:
                equal = same(event.getUInt32(attributeName), source.getUInt32(attributeName));
                break;
            case UINT64:
                equal = event.getUInt64(attributeName).equals(source.getUInt64(attributeName));
                break;
            case BOOLEAN_ARRAY:
                equal = same(event.getBooleanArray(attributeName), source.getBooleanArray(attributeName));
                break;
            case BYTE_ARRAY:
                equal = same(event.getByteArray(attributeName), source.getByteArray(attributeName));
                break;
            case DOUBLE_ARRAY:
                equal = same(event.getDoubleArray(attributeName), source.getDoubleArray(attributeName));
                break;
            case FLOAT_ARRAY:
                equal = same(event.getFloatArray(attributeName), source.getFloatArray(attributeName));
                break;
            case INT16_ARRAY:
                equal = same(event.getInt16Array(attributeName), source.getInt16Array(attributeName));
                break;
            case INT32_ARRAY:
                equal = same(event.getInt32Array(attributeName), source.getInt32Array(attributeName));
                break;
            case INT64_ARRAY:
                equal = same(event.getInt64Array(attributeName), source.getInt64Array(attributeName));
                break;
            case IP_ADDR_ARRAY:
                equal = true;
                break;
            case STRING_ARRAY:
                equal = sameObjArr(event.getStringArray(attributeName), source.getStringArray(attributeName));
                break;
            case UINT16_ARRAY:
                equal = same(event.getUInt16Array(attributeName), source.getUInt16Array(attributeName));
                break;
            case UINT32_ARRAY:
                equal = same(event.getUInt32Array(attributeName), source.getUInt32Array(attributeName));
                break;
            case UINT64_ARRAY:
                equal = sameObjArr(event.getUInt64Array(attributeName), source.getUInt64Array(attributeName));
                break;
            case NBOOLEAN_ARRAY:
                equal = sameObjArr(event.getBooleanObjArray(attributeName), source.getBooleanObjArray(attributeName));
                break;
            case NBYTE_ARRAY:
                equal = sameObjArr(event.getByteObjArray(attributeName), source.getByteObjArray(attributeName));
                break;
            case NDOUBLE_ARRAY:
                equal = sameObjArr(event.getDoubleObjArray(attributeName), source.getDoubleObjArray(attributeName));
                break;
            case NFLOAT_ARRAY:
                equal = sameObjArr(event.getFloatObjArray(attributeName), source.getFloatObjArray(attributeName));
                break;
            case NINT16_ARRAY:
                equal = sameObjArr(event.getShortObjArray(attributeName), source.getShortObjArray(attributeName));
                break;
            case NINT32_ARRAY:
                equal = sameObjArr(event.getIntegerObjArray(attributeName), source.getIntegerObjArray(attributeName));
                break;
            case NINT64_ARRAY:
                equal = sameObjArr(event.getLongObjArray(attributeName), source.getLongObjArray(attributeName));
                break;
            case NSTRING_ARRAY:
                equal = sameObjArr(event.getStringObjArray(attributeName), source.getStringObjArray(attributeName));
                break;
            case NUINT16_ARRAY:
                equal = sameObjArr(event.getIntegerObjArray(attributeName), source.getIntegerObjArray(attributeName));
                break;
            case NUINT32_ARRAY:
                equal = sameObjArr(event.getLongObjArray(attributeName), source.getLongObjArray(attributeName));
                break;
            case NUINT64_ARRAY:
                equal = sameObjArr(event.getBigIntegerObjArray(attributeName), source.getBigIntegerObjArray(attributeName));
                break;
            default: 
                equal = true;
            }
            
            if(!equal){
                System.out.println("Failed to match the attribute " + attributeName);
                return false;
            }
        }
        
        return true;
    }

    public void destroy() {
        
    }
    
}