import java.util.*;
import java.io.*;
import java.lang.*;

public class Passenger extends User {
  int money;

  Passenger(String user, String pass) {
    super(user, pass);
    this.money = 200;
  }

  public void payTrip(int cost) {
    this.money -= cost;
  }
}