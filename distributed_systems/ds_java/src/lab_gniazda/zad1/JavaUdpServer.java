package lab_gniazda.zad1;

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


                InetAddress senderAddress = receivePacket.getAddress(); // wez adres
                int senderPort = receivePacket.getPort();
                // zamiana kodowania na utf-8, zadanie 2
                String msg = new String(receivePacket.getData(), 0, receivePacket.getLength(), StandardCharsets.UTF_8);
                System.out.println("received msg: " + msg);

                // wyślij odpowiedź do nadawcy
                byte[] sendBuffer = "Pong Java Udp".getBytes(StandardCharsets.UTF_8);
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
