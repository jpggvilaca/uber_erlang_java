import java.util.*;
import java.io.*;
import java.lang.*;

public class Driver {
  public int[] Home;
  public String Model;
  public String Licence;
  public int TripsMade;
  public int MoneyReceived;
  public String Username;
  protected String Password;

  Driver(String user, String pass, String l, String m, int home[]) {
    this.Username = user;
    this.Password = pass;
    if(home.length > 0) {
      System.arraycopy( home, 0, this.Home, 0, 2 );
    }

    else {
      this.Home = new int[2];
    }

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