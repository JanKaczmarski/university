package lab_gniazda.zad1;

public class Main {
    public static void main(String[] args) {
        new Thread(() -> {
            try {
                new JavaUdpServer().start();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }).start();

        try { Thread.sleep(500); } catch (InterruptedException e) { e.printStackTrace(); }

        new Thread(() -> {
            try {
                new JavaUdpClient().start();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }).start();
    }
}
