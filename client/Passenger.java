import java.util.*;
import java.io.*;
import java.lang.*;

public class Passenger {
  public String Username;
  protected String Password;
  int money;

  Passenger(String user, String pass) {
    this.money = 200;
    this.Password = pass;
    this.Username = user;
  }

  public void payTrip(int cost) {
    this.money -= cost;
  }
}