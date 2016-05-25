import java.io.*;
import java.net.*;

public class ReceiverTwo extends Thread {
  Socket socket;
  ReceiverTwo(Socket socket) {
    this.socket = socket;
  }

  public void run() {
    try {
      PrintWriter writer = new PrintWriter(socket.getOutputStream());

      writer.flush();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}