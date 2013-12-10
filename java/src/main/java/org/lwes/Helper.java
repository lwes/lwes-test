package org.lwes;

import java.lang.reflect.Array;

public class Helper {
    
    static boolean same(long a, long b){
        return a == b;
    }
    
    static boolean same(double a, double b){
        if(Math.abs(a-b) > 0.0004)
            return false;
        else
            return true;
    }
    
    static boolean sameArrSize(Object a, Object b){
        if(a==null && b!=null
           || a!=null && b==null)
            return false;
        if(a==null && b==null)
            return true;
        return Array.getLength(a) == Array.getLength(b);
    }
    
    static boolean same(long[] a, long[] b){
        if(sameArrSize(a, b)){
            for(int i=0; i<a.length; i++){
                if(a[i] != b[i])
                    return false;
            }
            return true;
        }
        return false;
    }
    
    static boolean same(int[] a, int[] b){
        if(sameArrSize(a, b)){
            for(int i=0; i<a.length; i++){
                if(a[i] != b[i])
                    return false;
            }
            return true;
        }
        return false;
    }
    
    static boolean same(short[] a, short[] b){
        if(sameArrSize(a, b)){
            for(int i=0; i<a.length; i++){
                if(a[i] != b[i])
                    return false;
            }
            return true;
        }
        return false;
    }
    
    static boolean same(byte[] a, byte[] b){
        if(sameArrSize(a, b)){
            for(int i=0; i<a.length; i++){
                if(a[i] != b[i])
                    return false;
            }
            return true;
        }
        return false;
    }
    
    static boolean same(float[] a, float[] b){
        if(sameArrSize(a, b)){
            for(int i=0; i<a.length; i++){
                if(!same(a[i], b[i]))
                    return false;
            }
            return true;
        }
        return false;
    }
    
    static boolean same(double[] a, double[] b){
        if(sameArrSize(a, b)){
            for(int i=0; i<a.length; i++){
                if(!same(a[i], b[i]))
                    return false;
            }
            return true;
        }
        return false;
    }
    
    static boolean same(boolean[] a, boolean[] b){
        if(sameArrSize(a, b)){
            for(int i=0; i<a.length; i++){
                if(a[i] != b[i])
                    return false;
            }
            return true;
        }
        return false;
    }
    
    static <T> boolean sameObjArr(T[] a, T[] b){
        if(sameArrSize(a,b)){
            for(int i=0; i<a.length; i++){
                if(a[i] ==null && b[i] == null)
                    continue;
                
                if(a[i] == null && b[i] !=null || 
                   a[i] !=null && b[i] == null)
                    return false;
                   
                if(!a[i].equals(b[i]))
                    return false;
            }
            return true;
        }
        return false;
    }
    
}
