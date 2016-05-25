import java.io.*;
import java.net.*;

public class TransmitterTwo extends Thread {
  Socket socket;
  TransmitterTwo(Socket socket) {
    this.socket = socket;
  }

  public void run() {
    try {
      String message = null;
      BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      while ((message = reader.readLine()) != null) {
        System.out.println("Client message: "+ message);
      }

      socket.close();
    } catch(IOException e) {
      e.printStackTrace();
    }
  }
}