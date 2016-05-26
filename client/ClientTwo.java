import java.util.*;
import java.io.*;
import java.net.*;

public class ClientTwo {
  public static void introMessage() {
    System.out.println("Bemvindo ao uber!\n");
    System.out.println("Registo condutor - 1:reg:username:password:type:model:license");
    System.out.println("Registo passageiro - 1:reg:username:password:type");
    System.out.println("Login - 1:log:username:password:type");
    System.out.println("Sair - quit");
  }

  public static void preTripMessage(boolean isDriver) {
    if(isDriver)
      System.out.println("Disponível para conduzir: 2:can_drive:x:y");
    else
      System.out.println("Pedido de viagem: 2:want_trip:x1:y1:x2:y2");
  }

  public static void tripMessage() {
    System.out.println("Começar viagem: start_trip");
    System.out.println("Cancelar viagem: cancel_trip");
  }

  public static void register(String message) {
    if (message.equals("register_ok\n")) {
      System.out.println("Registo efectuado com sucesso!\n");
    }

    else if (message.equals("register_failed\n")) {
      System.out.println("Registo falhou! Por favor tente novamente.\n");
    }
  }

  public static void login(String message) {
    if (message.equals("login_ok\n")) {
      System.out.println("Login efectuado com sucesso!\n");
    }

    else if (message.equals("login_failed_wrong_password\n")) {
      System.out.println("Password errada! Por favor tente novamente.\n");
    }

    else if (message.equals("login_failed_user_doesnt_exist\n")) {
      System.out.println("Utilizador não existe! Por favor tente novamente.\n");
    }
  }

  public static void preTrip(String message) {
    if (message.equals("driver_added\n")) {
      System.out.println("Condutor adicionado ao uber!\n");
    }

    else if (message.equals("passenger_added\n")) {
      System.out.println("Passageiro adicionado ao uber!\n");
    }
  }

  public static void main(String[] args) throws Exception {
    // Aux variables
    String socketMessage;
    String command;
    int step = 0; // 1 = register/login, 2 = pre-trip, 3 = trip
    boolean isDriver = false;

    // Init connection
    if (args.length<2)
      System.exit(1);

    // Init socket
    String host = args[0];
    int port = Integer.parseInt(args[1]);
    Socket socket = new Socket(host, port);

    // Init Streams
    PrintWriter printer = new PrintWriter(socket.getOutputStream(), true);
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    TransmitterTwo trans = new TransmitterTwo(socket);
    trans.start(); // Gets the message from socket

    // Init menu
    introMessage();

    while(true) {
      String readerInput = reader.readLine(); // Read from console
      printer.println(readerInput.trim()); // Sends it to the socket

      while(trans.getMessageFromSocket() == null);
      socketMessage = trans.getMessageFromSocket();

      if(socketMessage.equals("driver_added\n"))
        isDriver = true;

      switch(step) {
        case 1: preTripMessage(isDriver);
        break;
        case 2: tripMessage();
        break;
      }

      System.out.println("socketMessage: " + socketMessage);
      command = socketMessage.substring(0, 3);

      if(command == "1:r") { // Register
        register(socketMessage);
        step = 1;
      }
      else if(command == "1:l") { // Login
        login(socketMessage);
        step = 1;
      }
    }
  }
}