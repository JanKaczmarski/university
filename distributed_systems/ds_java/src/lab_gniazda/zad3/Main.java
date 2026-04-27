package lab_gniazda.zad3;

public class Main {
    public static void main(String[] args) {
        new Thread(() -> {
            try {
                new JavaUdpServer().start();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }).start();
    }
}
