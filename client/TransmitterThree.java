import java.util.*;
import java.util.concurrent.*;
import java.lang.*;
import java.io.*;
import java.net.*;

public class TransmitterThree extends Thread {
  final Socket socket;
  final BlockingQueue<String> queue;

  TransmitterThree(Socket socket, BlockingQueue<String> queue) {
    this.socket = socket;
    this.queue = queue;
  }

  public void run() {
    try {
      String response = null;
      BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      while ((response = reader.readLine()) != null) {
        System.out.println("Server response: " + response);
        // Add the response from the server to the queue
        queue.add(response);
      }

      socket.close();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}