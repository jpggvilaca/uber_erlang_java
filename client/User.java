import java.util.*;
import java.io.*;

public class User {
  public String Username;
  protected String Password;

  User(String username, String password) {
    this.Username = username;
    this.Password = password;
  }

  public void setPassword(String newpass) {
    this.Password = newpass;
  }

  public String getPassword() {
    return this.Password;
  }
}