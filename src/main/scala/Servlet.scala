package prnd;
import org.scalatra.ScalatraServlet
import org.scalatra.UrlGenerator.url
import java.net.URL
import org.scalatra.scalate.ScalateSupport
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import org.squeryl.{Table, KeyedEntity}

class Servlet extends ScalatraServlet with ScalateSupport {
	// Instantiate database connection through singleton construction
	// Without this Squeryl will fail to perform database operations
	Database.getClass()
	def dump(title:String, data:String) = {
		contentType = "text/html"
		layoutTemplate("dummy", "title"->title, "body"->data)
	}
	val consoleWriter = new java.io.OutputStreamWriter(java.lang.System.out, "cp866")
	def printConsole(data: String) {
		consoleWriter.write(data+"\n")
		consoleWriter.flush
	}
	def error(message:String, reason:RuntimeException) = {
		contentType = "text/html"
		layoutTemplate("error", "it" -> message, "error" -> reason)
	}
	def getId: Int = {
		params("id").toInt
	}
	notFound {
		// Try to render a ScalateTemplate if no route matched
		findTemplate(requestPath) map { path =>
			contentType = "text/html"
			layoutTemplate(path)
		} orElse
		serveStaticResource() getOrElse 
		resourceNotFound() 
	}
	def deleteRequestEntries[K, T<:KeyedEntity[K]](prefix:String, table:Table[T], allowed:Iterable[T]) {
		for (s <- allowed) {
			val fieldName = prefix+s.id
			val valStr = params.getOrElse(fieldName, "off")
			if (valStr=="on" || valStr=="ON") {
				table.delete(s.id)
			}
		}
	}
}
