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

    // Init buffers for reading and writing to the socket; Init Transmitters
    BufferedReader input =
      new BufferedReader(new InputStreamReader(sock.getInputStream()));
    PrintWriter output = new PrintWriter(sock.getOutputStream());
    Transmitter trans1 = new Transmitter(input, output);
    Transmitter trans2 = new Transmitter(input, output);
    // User utilizador;

    // Init Scanners
    Scanner first = new Scanner(System.in); // Login-Register
    Scanner car = new Scanner(System.in); // Model-Licence
    Scanner second = new Scanner(System.in); // Trip

    // Auxiliary variables
    boolean isDriver = false;
    boolean step1 = false; // Login-Register
    boolean step2 = false; // Trip
    String step1_option; // Login or register
    String step2_option; // Trip
    String parsedOption[] = new String[4]; // Login-Register
    String parsedOption_2[] = new String[3]; // Trip

    // Input variables
    String username, password, type, model, licence;

    // Tests
    String testdriver[] = new String[3];
    String testpassenger[] = new String[3];
    testdriver[0] = "1:reg:condutor:pass:1:seat:ibiza";
    testdriver[1] = "1:log:condutor:pass:1";
    testdriver[2] = "2:can_drive:2:3";

    testpassenger[0] = "1:reg:passageiro:passcenas:1";
    testpassenger[1] = "1:log:passageiro:passcenas:1";
    testpassenger[2] = "2:want_trip:6:6:5:2";

    // Menu init
    System.out.println("Bemvindo ao uber!\n");
    System.out.println("Registo - register:username:password:type");
    System.out.println("Login - login:username:password");
    System.out.println("Sair - quit");



    // STEP1 LOOP - REGISTER/LOGIN
    while(!step1) {
      System.out.println("\nOpçao: ");
      step1_option = first.nextLine();
      parsedOption = step1_option.split(":");

      switch (parsedOption[0]) {
        case "test-1":
          for(int i= 0; i < 3; i++) {
            trans1.transmit(testdriver[i]);
            trans1.receive();
            isDriver = true;
            step1 = true;
          }
        break;

        case "test-2":
          for(int i= 0; i < 3; i++) {
            trans1.transmit(testpassenger[i]);
            trans1.receive();
            isDriver = false;
            step1 = true;
          }
        break;

        case "register":
          // Get User input, parse it, and send it to the socket
          if(new Integer(parsedOption[3]) == 1) { // If it's a driver
            System.out.println("Veiculo - modelo:matricula");

            String answer[] = new String[2];
            String mobile = "";

            mobile = car.nextLine();
            answer = mobile.split(":");
            username = parsedOption[1];
            password = parsedOption[2];
            type = parsedOption[3];
            model = answer[0];
            licence = answer[1];

            trans1.transmit("1:reg:"+username+":"+password+":"+type+":"+model+":"+licence);
            // utilizador = new User(username, password, type);
            // utilizador.setCar(model, licence);
          }

          else {
            trans1.transmit("1:reg:"+parsedOption[1]+":"+parsedOption[2]+":"+parsedOption[3]);
            // utilizador = new User(parsedOption[1], parsedOption[2], parsedOption[3]);
          }

          // Receive from the socket and output message
          trans1.receive();
          String result = trans1.getOutput();

          while(result == null);
          if(result == "register_ok\n") {
            System.out.println("Registo efectuado com sucesso!\n");
            System.out.println("Por favor faça login");
          }
          else if (result == "register_failed\n") {
            System.out.println("Registo falhou! Tente novamente.");
          }

          break;
        case "login":
          trans1.transmit("1:log:"+parsedOption[1]+":"+parsedOption[2]+":"+parsedOption[3]);

          trans1.receive();
          // result = trans1.getOutput();

          while(trans1.getOutput() == null);
          if(trans1.getOutput() == "login_ok\n") {
            System.out.println("Login efectuado com sucesso!\n");
          }
          else if (trans1.getOutput() == "login_failed\n") {
            System.out.println("Login falhou! Tente novamente.");
          }

          System.out.println("Bemvindo " + parsedOption[1]);

          if(new Integer(parsedOption[3]) == 1) { // If his type is equal to 1
            System.out.println("Condutor, escreva 'can_drive' quando tiver disponível para conduzir\n");
          }

          else {
            System.out.println("Passageiro, escreva 'want_trip' quando quiser viajar\n");
          }

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
          System.out.println("Uber says bye\n");
          System.exit(0);
      }
    }

    // STEP2 LOOP - REQUEST TRIP / AVAILABLE TO DRIVE
    while(!step2) {
      if(!isDriver) {
        System.out.println("\nFormato Viagem -> deX:deY:paraX:paraY (e.g. 1:2:2:4)");
        System.out.println("\nPassageiro - Coordenadas: ");

        step2_option = second.nextLine();
        parsedOption_2 = step2_option.split(":");
        trans2.transmit("2:want_trip:"+parsedOption_2[0]+":"+parsedOption_2[1]+":"+parsedOption_2[2]+":"+parsedOption_2[3]);
        trans2.receive();
        while(trans2.getOutput() == null);
        System.out.println(trans2.getOutput());

        System.out.println("\nCancelar viagem (cancel_trip): ");
      }

      else {
        System.out.println("\nCondutor - Coordenadas da sua casa (x:y): ");

        step2_option = second.nextLine();
        parsedOption_2 = step2_option.split(":");
        // utilizador.setHome(parsedOption_2);

        trans2.transmit("2:can_drive:"+parsedOption_2[0]+":"+parsedOption_2[1]);

        trans2.receive();
        while(trans2.getOutput() == null);
        System.out.println(trans2.getOutput());
      }
    }
  }
}