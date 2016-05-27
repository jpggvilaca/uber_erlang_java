import java.util.*;
import java.util.concurrent.*;
import java.lang.*;
import java.io.*;
import java.net.*;

public class ClientThree {
  public static void introMessage() {
    System.out.println("Bemvindo ao uber!\n");
    System.out.println("Registo condutor - 1:reg:username:password:type:model:license");
    System.out.println("Registo passageiro - 1:reg:username:password:type");
    System.out.println("Login - 1:log:username:password:type");
    System.out.println("Sair - quit");
  }

  public static void register(String message) {
    if (message.equals("register_ok")) {
      System.out.println("Registo efectuado com sucesso!\n");
      System.out.println("Por favor faça login.");
    }

    else if (message.equals("register_failed")) {
      System.out.println("Registo falhou! Por favor tente novamente.");
    }
  }

  public static void login(String message) {
    if (message.equals("login_ok")) {
      System.out.println("Login efectuado com sucesso!");
    }

    else if (message.equals("login_failed_wrong_password")) {
      System.out.println("Password errada! Por favor tente novamente.");
    }

    else if (message.equals("login_failed_user_already_exists")) {
      System.out.println("Utilizar já tem sessão inicada.");
    }

    else if (message.equals("login_failed_user_doesnt_exist")) {
      System.out.println("Utilizador não existe! Por favor tente novamente.");
    }
  }

  public static void added(String message) {
    if (message.equals("driver_added")) {
      System.out.println("Condutor adicionado ao uber!");
    }

    else if (message.equals("passenger_added")) {
      System.out.println("Passageiro adicionado ao uber!");
    }
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

  public static void main(String[] args) throws Exception {
    // Aux variables
    String socketMessage;
    String command;
    String aux; // To check if it's driver or passenger
    int step = 0; // 1 = register/login, 2 = pre-trip, 3 = trip
    boolean isDriver = true;

    // Init connection
    if (args.length<2)
      System.exit(1);

    // Init socket
    String host = args[0];
    int port = Integer.parseInt(args[1]);
    Socket socket = new Socket(host, port);

    // Init Streams
    BlockingQueue<String> messages = new LinkedBlockingQueue<>();
    PrintWriter printer = new PrintWriter(socket.getOutputStream(), true);
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    TransmitterThree trans = new TransmitterThree(socket, messages);
    trans.start(); // Gets the message from socket

    // Init menu
    introMessage();

    while(true) {
      String readerInput = reader.readLine(); // Read from console
      printer.print(readerInput.trim()); // Sends it to the socket
      printer.flush();

      socketMessage = messages.take(); // Get the server response
      command = readerInput.substring(0, 3); // Get the parsed string from user input
      aux = readerInput.substring(readerInput.length() - 1, readerInput.length());

      if(socketMessage.equals("driver_arrived")) {
        tripMessage();
      }

      switch(command) {
        case "1:r":
          register(socketMessage);
          if(socketMessage.equals("register_ok")) {
            step = 1;
          }
        break;
        case "1:l":
          if(socketMessage.equals("login_ok")) {
            login(socketMessage);
            if(aux.equals("2"))
              isDriver = false;
            preTripMessage(isDriver);
          }
          else {
            login(socketMessage);
          }
        break;
        case "2:c":
          if(socketMessage.equals("driver_added")) {
            System.out.println("Foi adicionado à lista de condutores.");
          }
        break;
        case "2:w":
          if(socketMessage.equals("passenger_added")) {
            isDriver = false;
            System.out.println("Foi adicionado à lista de passageiros.");
          }
        break;
        case "sta":
          System.out.println("Viagem começou!");
        break;
        case "can":
          System.out.println("Viagem cancelada!");
        break;
        case "qui":
          System.out.println("Adeus.");
          System.exit(0);
        break;
        default:
          System.out.println("Por favor insira um comando válido.");
        break;
      }
    }
  }
}