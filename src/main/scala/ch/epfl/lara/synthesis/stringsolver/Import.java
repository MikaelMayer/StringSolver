package ch.epfl.lara.synthesis.stringsolver;
import java.net.*;
import java.io.*;

public class Import
{
static ServerSocket serverSocket = null;
static boolean running = true;


public static void start(String arg[]) {   
   Socket client = null;
   try {
      System.out.println("Starting service on IP Address "+
         InetAddress.getLocalHost().getHostAddress()+
         " port No.12345");
      serverSocket = new ServerSocket(12345);
      }
   catch(Exception ex) { ex.printStackTrace(); }
   byte b[] = new byte[256];
   String tmp = null;
   while(running && !serverSocket.isClosed()) {
      try {
         System.out.println("Listening for clients on IP Address "+
            InetAddress.getLocalHost().getHostAddress()+
            " port No.12345");
         client = serverSocket.accept();
       System.out.println("Client from "+client.getInetAddress()+":"+
client.getPort()+" connected at "+(new java.util.Date().toString()));
         int n = client.getInputStream().read(b);
         tmp = new String(b, 0, n).replaceAll(",","");
         double x = Double.parseDouble(tmp);
         double y = x*x;
         client.getOutputStream().write((""+y).getBytes());
         client.getOutputStream().flush();
         System.out.println("Received "+x+" and sent "+y);
         client.close();
         }
      catch(Exception ex) { ex.printStackTrace(); }
      }
   }

public static void stop(String arg[]) {
   if(!running) return;
   running = false;
try { serverSocket.close(); } catch(Exception ex) { ex.printStackTrace(); }
   System.out.println("Stopped service");
   }
}
