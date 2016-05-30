package highperfscala.dataanalysis.util

import java.io._

import highperfscala.dataanalysis.Execution
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer

object DataCodec extends Logging {

  def write(cs: List[Execution], output: File): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(output))
    cs.foreach(oos.writeObject)
    oos.close()
  }

  def read(input: File): List[Execution] = {
    val fis = new FileInputStream(input)

    val ois = new ObjectInputStream(fis)
    val commandBuilder = ListBuffer[Execution]()
    while(fis.available() != 0) {
      commandBuilder.append(ois.readObject().asInstanceOf[Execution])
    }
    ois.close()
    fis.close()

    commandBuilder.result()
  }

  def read(input: InputStream): List[Execution] = {
    val ois = new ObjectInputStream(input)
    val commandBuilder = ListBuffer[Execution]()
    while(input.available() != 0) {
      commandBuilder.append(ois.readObject().asInstanceOf[Execution])
    }
    ois.close()

    commandBuilder.result()
  }

}
