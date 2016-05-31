import java.util.*;
import java.util.concurrent.*;
import java.lang.*;
import java.io.*;
import java.net.*;

public class FakeUser extends Thread {
  final String Users[] = {"Joao", "Rui", "Pedro", "Paulo", "Eduardo", "Vasco", "Marcio", "Nelson", "Hugo"}; // 9
  final String Passwords[] = {"mypass", "safepass", "lolpass", "fakepass", "securepass", "worldclasspass"}; // 6
  final String Models[] = {"opel", "seat", "citroen", "bmw", "mercedes", "renault", "fiat"}; // 7
  final String Licences[] = {"corsa", "ibiza", "serie5", "leon", "toledo", "tigra", "slk", "serie4", "punto"}; // 9

  FakeUser() {};

  public String[] driver(String username, String password, String model, String licence, int x, int y) {
    String message[] = new String[3];

    message[0] = "1:reg:" + username + ":" + password + ":1:" + model + ":" + licence;
    message[1] = "1:log:" + username + ":" + password + ":1";
    message[2] = "2:can_drive:" + x + ":" + y;

    return message;
  }

  public String[] passenger(String username, String password, int FromX, int FromY, int ToX, int ToY) {
    String message[] = new String[3];

    message[0] = "1:reg:" + username + ":" + password + ":2";
    message[1] = "1:log:" + username + ":" + password + ":2";
    message[2] = "2:want_trip:" + FromX + ":" + FromY + ":" + ToX + ":" + ToY;

    return message;
  }

  public void run() {
    try {
      String host = "127.0.0.1";
      int port = 8888;
      Socket socket = new Socket(host, port);

      // Init Streams
      BlockingQueue<String> messages = new LinkedBlockingQueue<>(); // Message Queue
      PrintWriter printer = new PrintWriter(socket.getOutputStream(), true); // Prints to the socket
      Transmitter trans = new Transmitter(socket, messages); // Receives messages from the socket and add them to the queue
      Receiver rec = new Receiver(messages); // Retrieves messages from the socket

      trans.start();
      rec.start();

      Random generator = new Random();
      String testMessage[] = new String[3];

      String UserName = Users[generator.nextInt(9)];
      String Password = Passwords[generator.nextInt(6)];
      String Model = Models[generator.nextInt(7)];
      String Licence = Licences[generator.nextInt(9)];
      int X = generator.nextInt(10);
      int Y = generator.nextInt(10);
      int FromX = generator.nextInt(10);
      int FromY = generator.nextInt(10);
      int ToX = generator.nextInt(10);
      int ToY = generator.nextInt(10);

      if (generator.nextInt(3) < 1) { // choose driver or passenger
        System.arraycopy( driver(UserName, Password, Model, Licence, X, Y), 0, testMessage, 0, driver(UserName, Password, Model, Licence, X, Y).length );
      }

      else {
        System.arraycopy( passenger(UserName, Password, FromX, FromY, ToX, ToY), 0, testMessage, 0, passenger(UserName, Password, FromX, FromY, ToX, ToY).length );
      }

      for(int a = 0; a < 3; a++) {
        printer.print(testMessage[a]);
        printer.flush();
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}