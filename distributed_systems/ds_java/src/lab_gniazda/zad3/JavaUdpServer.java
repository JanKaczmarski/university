package lab_gniazda.zad3;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.nio.ByteBuffer;
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

                // zamiana bajty -> int (big-endian)
                byte[] buff = Arrays.copyOf(receivePacket.getData(), receivePacket.getLength());
                int nb = ByteBuffer.wrap(buff).getInt();
                System.out.println("received number: " + nb);

                // zwiększ o jeden i odeślij jako bajty
                buff = ByteBuffer.allocate(4).putInt(nb + 1).array();
                DatagramPacket sendPacket = new DatagramPacket(buff, buff.length, senderAddress, senderPort);
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
