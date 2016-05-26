import java.io.*;
import java.net.*;

public class TransmitterTwo extends Thread {
  Socket socket;
  String message;

  TransmitterTwo(Socket socket) {
    this.socket = socket;
  }

  public String getMessageFromSocket() {
    return message;
  }

  public void run() {
    try {
      String response = null;
      BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      while ((response = reader.readLine()) != null) {
        System.out.println("Server response: "+ response);
        this.message = response;
      }

      socket.close();
    } catch(IOException e) {
      e.printStackTrace();
    }
  }
}