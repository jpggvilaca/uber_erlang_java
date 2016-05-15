import java.util.*;
import java.io.*;
import java.lang.*;

public class Driver extends User {
  public int[] Home;
  public String Model;
  public String Licence;
  public int TripsMade;
  public int MoneyReceived;

  Driver(String user, String pass, int[] home, String l, String m) {
    super(user, pass);
    System.arraycopy( home, 0, this.Home, 0, home.length );
    this.TripsMade = 0;
    this.MoneyReceived = 0;
    this.Model = m;
    this.Licence = l;
  }

  public void makeTrip(int Money) {
    this.TripsMade += 1;
    this.MoneyReceived += Money;
  }
}