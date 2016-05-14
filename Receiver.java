import java.util.*;
import java.io.*;
import java.net.*;

class Receiver implements Runnable {
  BufferedReader in;
  PrintWriter out;

  Receiver(BufferedReader in, PrintWriter out) {
    this.in = in;
    this.out = out;
  }

  boolean quit = false;
  int option;
  Scanner menu = new Scanner(System.in);
  Scanner clientReader = new Scanner(System.in);

  public void run() {
    try {
      while(!quit) {
        System.out.println("\nOpçao: ");
        option = menu.nextInt();
        switch (option) {
          case 1:
            System.out.println("Insira os dados:");

            out.print(clientReader.nextLine());
            out.flush();

            while((in.readLine()) != null) {
              System.out.println(in.readLine());

              if (in.readLine().equals("\n"))
                break;
            }

            break;
          case 2:
            System.out.println("Login");
            System.out.println("Insira o número 2, username e password separado por espaços");
            System.out.println("Exemplo: '2 gen baderous'\n");
            out.print(clientReader.nextLine());
            break;
          case 3:
            System.out.println("Solicitar viagem");
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}