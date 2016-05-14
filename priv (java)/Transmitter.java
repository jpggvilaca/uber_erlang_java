import java.util.*;
import java.io.*;
import java.net.*;

class Transmitter {
  BufferedReader input;
  PrintWriter output;

  Transmitter(BufferedReader in, PrintWriter out) {
    this.input = in;
    this.output = out;
  }

  public void transmit(Socket socket, String message) throws Exception {
    // Sends to the socket
    Thread speaker = new Thread(new Runnable() {
      public void run() {
        try {
          // System.out.println(message);
          output.print(message);
          output.flush();

        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    });

    speaker.start();
  }

  public void receive(Socket socket) throws Exception {
    // Reads from the socket
    Thread listener = new Thread(new Runnable() {
      public void run() {
        String res;

        try {
          while((res = input.readLine()) != null) {
            // System.out.println(res);

            if (res.equals("\n"))
              break;
           }
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    });

    listener.start();
  }
}