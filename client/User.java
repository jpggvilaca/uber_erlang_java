import java.util.*;
import java.io.*;
import java.lang.*;

public class User {
  // Driver
  public int[] Home;
  public String Model;
  public String Licence;

  // General
  public String Type;
  public String Username;
  protected String Password;

  User() {}

  User(String user, String pass, String type) {
    this.Username = user;
    this.Password = pass;
    this.Type = type;
  }

  public void setHome(String home[]) {
    if(home.length > 0) {
      System.arraycopy( home, 0, this.Home, 0, 2 );
    }

    else {
      this.Home = new int[2];
    }
  }

  public void setCar(String model, String licence) {
    this.Model = model;
    this.Licence = licence;
  }
}