import java.util.*;
import java.io.*;
import java.net.*;

public class Client {

  public static void main(String[] args) throws Exception {
    // Init connection
    if (args.length<2)
      System.exit(1);

    String host = args[0];
    int port = Integer.parseInt(args[1]);
    Socket sock = new Socket(host, port);

    // Init buffers for reading and writing to the socket
    BufferedReader input =
      new BufferedReader(new InputStreamReader(sock.getInputStream()));
    PrintWriter output = new PrintWriter(sock.getOutputStream());
    Transmitter trans = new Transmitter(input, output);

    // Init Scanners
    Scanner first = new Scanner(System.in); // Login-Register
    Scanner car = new Scanner(System.in);
    Scanner second = new Scanner(System.in); // Trip

    // Auxiliary variables
    boolean isDriver = false;
    boolean step1 = false; // Login-Register
    boolean step2 = false; // Trip
    String step1_option; // Login or register
    String step2_option; // Trip
    String parsedOption[] = new String[3];
    String parsedOption_2[] = new String[2];
    System.out.println("Bemvindo ao uber!\n");
    System.out.println("Nota: type = 1 ou 2\n");
    System.out.println("Registo - register:username:password:type");
    System.out.println("Login - login:username:password");
    System.out.println("Solicitar viagem - request_trip");
    System.out.println("Sair - quit");


    // step1 loop - register/login
    while(!step1) {
      System.out.println("\nOpçao: ");
      step1_option = first.nextLine();
      parsedOption = step1_option.split(":");

      switch (parsedOption[0]) {
        case "register":
          if(new Integer(parsedOption[3]) == 1) { // If it's a driver
            System.out.println("Veiculo - modelo:matricula\n");
            String answer[] = new String[2];
            String mobile = "";
            mobile = car.nextLine();
            answer = mobile.split(":");
            trans.transmit(sock, "reg:"+parsedOption[1]+":"+parsedOption[2]+":"+parsedOption[3]+":"+answer[0]+":"+answer[1]);
          }

          else {
            System.out.println("entrei aqui\n");
            System.out.println(parsedOption[0]);
            trans.transmit(sock, "reg:"+parsedOption[1]+":"+parsedOption[2]+":"+parsedOption[3]);
          }

          trans.receive(sock);

          break;
        case "login":
          trans.transmit(sock, "log:"+parsedOption[1]+":"+parsedOption[2]+":"+parsedOption[3]);
          trans.receive(sock);

          break;
        case "want_trip":
          step1 = true;
          isDriver = false;

          break;
        case "can_drive":
          step1 = true;
          isDriver = true;

          break;
        case "quit":
          System.out.println("Bye bye");
          System.exit(0);
      }
    }

    // step2 loop - request trip / available to drive
    while(!step2) {
      if(!isDriver) {
        System.out.println("\nFormato -> de:para (e.g. (1,2):(2,4))");
        System.out.println("\nPassageiro - Opçao: ");
        step2_option = second.nextLine();
        parsedOption_2 = step2_option.split(":");
        trans.transmit(sock, "trip:want_trip:"+parsedOption_2[0]+":"+parsedOption_2[1]);
      }

      else {
        System.out.println("\nCondutor - Opçao: ");
        step2_option = second.nextLine();
        parsedOption_2 = step2_option.split(":");
        trans.transmit(sock, "trip:can_drive:"+parsedOption_2[0]+":"+parsedOption_2[1]);
      }

      trans.receive(sock);
    }
  }
}