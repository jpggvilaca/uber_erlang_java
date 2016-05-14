import java.util.*;
import java.io.*;
import java.net.*;

class Communication {
  Socket socket;
  BufferedReader in;
  PrintWriter out;
  Receiver r;

  public boolean connectToServer(String hostname, int port) {
   boolean r = true;

   try {
     this.socket = new Socket(hostname, port);
     this.in = new BufferedReader(new InputStreamReader(this.socket.getInputStream()));
     this.out = new PrintWriter(this.socket.getOutputStream(), true);
     this.r = new Receiver(this.in, this.out);
     new Thread(this.r).start();

   } catch(IOException ex){
     System.out.println(ex.getMessage());
     r = false;
   }
   return r;
  }

  public synchronized void send(String s) {
    try {
      out.println(s);
      out.flush();
    } catch(Exception e){}
  }
}