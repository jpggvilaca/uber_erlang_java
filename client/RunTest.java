

public class RunTest {
  public static void main(String[] args) {
    Thread testes[] = new Thread[10];

    for(int i = 0; i < 10; i++) {
      try {
        testes[i] = new FakeUser();
        testes[i].start();
        testes[i].join();
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
  }
}