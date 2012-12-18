package prnd;
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
 
//This object is for Debugging from within eclipse.
//Code was taken from http://acrosanctblood.blogspot.com/2012/01/run-jetty-in-eclipse-with-scalatra.html
object WebApp {
  def main(args: Array[String]) {
    val server = new Server(8080)
    val context: WebAppContext = new WebAppContext();
    context.setServer(server)
    context.setContextPath("/");
    context.setWar("src/main/webapp")
    server.setHandler(context);
 
    try {
      println(">>> STARTING EMBEDDED JETTY SERVER, PRESS ANY KEY TO STOP")
      server.start()
      server.join()
    } catch {
      case ex: Exception => {
        ex.printStackTrace()
        System.exit(1)
      }
    }
  }
}