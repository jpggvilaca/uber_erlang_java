import java.util.*;
import java.io.*;
import java.net.*;

public class Tests {
  public static void main(String[] args) throws Exception {
    Socket sock = new Socket("127.0.0.1", 8888);

    // Init buffers for reading and writing to the socket; Init Transmitters
    BufferedReader input =
      new BufferedReader(new InputStreamReader(sock.getInputStream()));
    PrintWriter output = new PrintWriter(sock.getOutputStream());
    Transmitter trans1 = new Transmitter(input, output);

    boolean isDriver = false;
    boolean step1 = false;
    boolean step2 = false;
    boolean step3 = false;
    String step1_option;
    String parsedOption[] = new String[4];

    Scanner first = new Scanner(System.in);

    String testdriver[] = new String[4];
    String testdriver2[] = new String[4];
    String testdriver3[] = new String[4];
    String testpassenger[] = new String[4];
    String testpassenger2[] = new String[4];
    String testpassenger3[] = new String[4];
    testdriver[0] = "1:reg:condutor:pass:1:seat:ibiza";
    testdriver[1] = "1:log:condutor:pass:1";
    testdriver[2] = "2:can_drive:2:3";

    testdriver2[0] = "1:reg:condutor2:pass2:1:opel:corsa";
    testdriver2[1] = "1:log:condutor2:pass2:1";
    testdriver2[2] = "2:can_drive:6:9";

    testdriver3[0] = "1:reg:condutor3:pass3:1:fiat:punto";
    testdriver3[1] = "1:log:condutor3:pass3:1";
    testdriver3[2] = "2:can_drive:1:4";

    testpassenger[0] = "1:reg:passageiro:passcenas:1";
    testpassenger[1] = "1:log:passageiro:passcenas:1";
    testpassenger[2] = "2:want_trip:6:6:5:2";
    testpassenger[3] = "start_trip";

    testpassenger2[0] = "1:reg:passageiro2:passcenas2:1";
    testpassenger2[1] = "1:log:passageiro2:passcenas2:1";
    testpassenger2[2] = "2:want_trip:1:6:0:3";
    testpassenger2[3] = "start_trip";

    testpassenger3[0] = "1:reg:passageiro3:passcenas3:1";
    testpassenger3[1] = "1:log:passageiro3:passcenas3:1";
    testpassenger3[2] = "2:want_trip:6:1:10:4";
    testpassenger3[3] = "start_trip";

    // Menu init
    System.out.println("Opção: \n");

    // STEP1 LOOP - REGISTER/LOGIN
    while(!step1) {
      System.out.println("\nOpçao: ");
      step1_option = first.nextLine();
      parsedOption = step1_option.split(":");

      switch (parsedOption[0]) {
        case "cond":
          for(int i= 0; i < 3; i++) {
            trans1.transmit(testdriver[i]);
            trans1.receive();
          }
        break;

        case "cond2":
          for(int i= 0; i < 3; i++) {
            trans1.transmit(testdriver2[i]);
            trans1.receive();
          }
        break;

        case "cond3":
          for(int i= 0; i < 3; i++) {
            trans1.transmit(testdriver3[i]);
            trans1.receive();
          }
        break;

        case "pass":
          for(int i= 0; i < 3; i++) {
            trans1.transmit(testpassenger[i]);
            trans1.receive();
          }
        break;

        case "pass2":
          for(int i= 0; i < 3; i++) {
            trans1.transmit(testpassenger2[i]);
            trans1.receive();
          }
        break;

        case "pass3":
          for(int i= 0; i < 3; i++) {
            trans1.transmit(testpassenger3[i]);
            trans1.receive();
          }
        break;
      }
    }
  }
}