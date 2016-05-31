import java.util.*;
import java.util.concurrent.*;
import java.util.regex.*;
import java.lang.*;
import java.io.*;
import java.net.*;

public class Client {
  public static void introMessage() {
    System.out.println("Bemvindo ao uber!\n");
    System.out.println("Registo condutor - 1:reg:username:password:type:model:license");
    System.out.println("Registo passageiro - 1:reg:username:password:type");
    System.out.println("Login - 1:log:username:password:type");
    System.out.println("Nota: type = 1 para condutor ou 2 para passageiro");
    System.out.println("Sair - quit");
  }

  public static void main(String[] args) throws Exception {
    // Init connection
    if (args.length<2)
      System.exit(1);

    // Init socket
    String host = args[0];
    int port = Integer.parseInt(args[1]);
    Socket socket = new Socket(host, port);

    // Init Streams
    BlockingQueue<String> messages = new LinkedBlockingQueue<>(); // Message Queue
    PrintWriter printer = new PrintWriter(socket.getOutputStream(), true); // Prints to the socket
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in)); // Reads from the console
    Transmitter trans = new Transmitter(socket, messages); // Receives messages from the socket and add them to the queue
    Receiver rec = new Receiver(messages); // Retrieves messages from the socket

    // Init Receiver and Transmitter
    trans.start();
    rec.start();

    // Init Menu and regexes
    introMessage();

    String patternOne = "[1-2]:[a-z]+:[a-z]+:[a-z]+:[1-2](:[a-z]*:[a-z]*)?";
    String patternTwo = "[1-2]:[a-z_]+:[0-9]+:[0-9]+(:[0-9]?:[0-9])?";

    while(true) {
      String readerInput = reader.readLine(); // Read from console

      if(readerInput.matches(patternOne) || readerInput.matches(patternTwo)) {
        printer.print(readerInput.trim()); // Sends it to the socket
        printer.flush();
      }

      else {
        System.out.println("Por favor introduza um comando válido.\n");
      }
    }
  }
}