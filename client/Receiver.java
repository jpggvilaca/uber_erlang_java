import java.util.*;
import java.util.concurrent.*;
import java.lang.*;
import java.io.*;
import java.net.*;

public class Receiver extends Thread {
  final BlockingQueue<String> queue;
  static boolean isDriver = true;

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
      isDriver = false;
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

        register(message);
        login(message);
        added(message);

        if(message.equals("user_is_passenger")) {
          isDriver = false;
        }

        if(message.equals("driver_arrived")) {
          tripMessage();
        }

        if(message.equals("cancel_trip") || message.equals("cancel_trip_before_time")) {
          System.out.println("Viagem cancelada!");
          preTripMessage(isDriver);
        }

        if(message.equals("login_ok"))
          preTripMessage(isDriver);

        if(message.equals("trip_ended")) {
          System.out.println("Viagem acabou!");
          preTripMessage(isDriver);
        }

        if(message.equals("no_drivers_available"))
          System.out.println("À espera de condutores disponíveis...");

        if (message.equals("driver_available"))
          System.out.println("Já há condutores disponíveis. Condutor a caminho...");

        if (message.substring(0, 4).equals("Info")) {
          String DriverInfo[] = new String[6];
          DriverInfo = message.split(":");
          System.out.println("Informação do condutor: ");
          System.out.println("Distância a que ele encontra: " + DriverInfo[1] + " unidades");
          System.out.println("Vai demorar a chegar " + DriverInfo[2] + " segundos");
          System.out.println("A viagem vai custar " + DriverInfo[3] + " euros");
          System.out.println("Modelo: " + DriverInfo[4]);
          System.out.println("Matricula: " + DriverInfo[5]);
          System.out.println("Cancelar viagem: cancel_trip");
        }
      }
    } catch (InterruptedException e) {
        e.printStackTrace();
    }
  }
}