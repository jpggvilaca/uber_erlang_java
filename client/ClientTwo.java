import java.util.*;
import java.io.*;
import java.net.*;

public class ClientTwo {

  public static void main(String[] args) throws Exception {
    String name = args[0];
    Socket socket = new Socket("localhost", 8888);

    PrintWriter printer = new PrintWriter(socket.getOutputStream(), true);
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    TransmitterTwo trans = new TransmitterTwo(socket);
    ReceiverTwo receiver = new ReceiverTwo(socket);
    trans.start();
    receiver.start();

    while(true) {
      String readerInput = reader.readLine();
      printer.println(name + ": "+readerInput);
    }
  }
}