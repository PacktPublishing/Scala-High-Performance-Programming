package highperfscala
package benchmarks

import java.io._
import java.net._

object IoExample {

  def main(args: Array[String]): Unit = {

    new Thread(new Runnable {
      def run(): Unit = {
        var messageCount = 0
        val welcomeSocket = new ServerSocket(6789)
        val connectionSocket = welcomeSocket.accept()
        val inFromClient = new BufferedReader(new InputStreamReader(
          connectionSocket.getInputStream()))
        while (messageCount < 10000) {
          val clientSentence = inFromClient.readLine()
          System.out.println("Received: " + clientSentence)
          messageCount = messageCount + 1
        }
      }
    }).start()

    Thread.sleep(100)

    val clientSocket = new Socket("localhost", 6789)
    val outToServer = new DataOutputStream(clientSocket.getOutputStream())
    val pw = new PrintWriter("/tmp/foobar")
    for (i <- 1 to 10000) {
      println("Writing message")
      outToServer.writeBytes("hello\n")
      Thread.sleep(1)
      pw.write("hello\n")
    }
    pw.close()
    clientSocket.close()
  }
}
