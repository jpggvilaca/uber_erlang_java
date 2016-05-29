import java.util.*;
import java.util.concurrent.*;
import java.lang.*;
import java.io.*;
import java.net.*;

public class Receiver extends Thread {
  final BlockingQueue<String> queue;
  boolean isDriver = true;

  Receiver(BlockingQueue<String> queue) {
    this.queue = queue;
  }

  // Auxiliary methods to show messages to the user
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

  public static void cleanScreen() {
    for (int i = 0; i < 50; ++i) System.out.println();
  }

  // Main job
  public void run() {
    try {
      String message = null;
      while (true) {
        // Gets the message from the queue
        message = queue.take();
        System.out.println("Taken from queue: " + message + "\n");

        register(message);
        login(message);
        added(message);

        if(message.equals("passenger_added")) {
          isDriver = false;
          tripMessage();
        }

        if(message.equals("login_ok"))
          preTripMessage(isDriver);

        if(message.equals("trip_ended"))
          System.out.println("Viagem acabou!");

        if(message.equals("no_drivers_available"))
          System.out.println("À espera de condutores disponíveis...");

        if (message.equals("drivers_available"))
          System.out.println("Já há condutores disponíveis. Condutor a caminho...");

        if (message.substring(0, 4).equals("Info")) {
          String DriverInfo[] = new String[6];
          DriverInfo = message.split(":");
          System.out.println("Informação do condutor: \n");
          System.out.println("Distância a que ele encontra: " + DriverInfo[1] + "\n");
          System.out.println("Vai demorar a chegar " + DriverInfo[2] + "segundos\n");
          System.out.println("A viagem vai custar " + DriverInfo[3] + "euros\n");
          System.out.println("Modelo: " + DriverInfo[4] + "\n");
          System.out.println("Matricula: " + DriverInfo[5] + "\n");
        }

        // switch(command) {
        //   case "1:r":
        //     if(socketMessage.equals("register_ok"))
        //       // cleanScreen();
        //     register(socketMessage);
        //   break;
        //   case "1:l":
        //     if(socketMessage.equals("login_ok")) {
        //       login(socketMessage);
        //       if(aux.equals("2"))
        //         isDriver = false;
        //       preTripMessage(isDriver);
        //     }
        //     else {
        //       // cleanScreen();
        //       login(socketMessage);
        //     }
        //   break;
        //   case "2:c":
        //     if(socketMessage.equals("driver_added")) {
        //       System.out.println("Foi adicionado à lista de condutores.");
        //     }
        //   break;
        //   case "2:w":
        //     if(socketMessage.equals("passenger_added")) {
        //       isDriver = false;
        //       System.out.println("Foi adicionado à lista de passageiros.");
        //       tripMessage();
        //     }
        //   break;
        //   case "sta":
        //     System.out.println("Viagem começou!");
        //   break;
        //   case "can":
        //     System.out.println("Viagem cancelada!");
        //   break;
        //   case "qui":
        //     System.out.println("Adeus.");
        //     System.exit(0);
        //   break;
        //   default:
        //     System.out.println("Por favor insira um comando válido.");
        //   break;
        // }
      }
    } catch (InterruptedException e) {
        e.printStackTrace();
    }
  }
}