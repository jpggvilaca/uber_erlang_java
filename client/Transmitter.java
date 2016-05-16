import java.util.*;
import java.util.concurrent.*;
import java.lang.*;
import java.io.*;
import java.net.*;

class Transmitter {
  BufferedReader input;
  PrintWriter output;
  String outputMessage;

  public String getOutput() {
    return outputMessage;
  }

  Transmitter(BufferedReader in, PrintWriter out) {
    this.input = in;
    this.output = out;
  }

  public void transmit(String message) throws Exception {
    // Sends to the socket
    Thread speaker = new Thread(new Runnable() {
      public void run() {
        try {
          System.out.println("speaker: " + message);
          output.print(message);
          output.flush();

        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    });

    speaker.start();
  }

  public void receive() throws Exception {
    // Reads from the socket
    Thread listener = new Thread(new Runnable() {
      public void run() {
        String res;

        try {
          while((res = input.readLine()) != null) {
            System.out.println("message received: " + res);

            outputMessage = (res);

            break;
           }
        } catch (IOException e) {
          e.printStackTrace();
        }
      };
    });

    listener.start();
    listener.join();
  }
}