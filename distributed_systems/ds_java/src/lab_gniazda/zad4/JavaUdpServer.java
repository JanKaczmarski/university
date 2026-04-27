package lab_gniazda.zad4;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public class JavaUdpServer {

    static final int portNumber = 9008;

    public void start() throws Exception {
        System.out.println("JAVA UDP SERVER");
        DatagramSocket socket = null;

        try {
            socket = new DatagramSocket(portNumber);
            byte[] receiveBuffer = new byte[1024];

            while (true) {
                Arrays.fill(receiveBuffer, (byte) 0);
                DatagramPacket receivePacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);
                socket.receive(receivePacket);

                // pobierz adres i port nadawcy z datagramu
                InetAddress senderAddress = receivePacket.getAddress();
                int senderPort = receivePacket.getPort();
                String msg = new String(receivePacket.getData(), 0, receivePacket.getLength(), StandardCharsets.UTF_8);
                System.out.println("received msg: " + msg);

                // rozpoznaj nadawcę po treści wiadomości i wyślij różną odpowiedź
                String response = msg.startsWith("Ping Java") ? "Pong Java" : "Pong Python";
                byte[] sendBuffer = response.getBytes(StandardCharsets.UTF_8);
                DatagramPacket sendPacket = new DatagramPacket(sendBuffer, sendBuffer.length, senderAddress, senderPort);
                socket.send(sendPacket);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (socket != null) {
                socket.close();
            }
        }
    }
}
