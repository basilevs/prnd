package prnd;
import org.scalatra.ScalatraServlet
import org.scalatra.UrlGenerator.url
import java.net.URL
import org.scalatra.scalate.ScalateSupport
import org.squeryl.{KeyedEntity, Session, Table}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.{ManyToMany}

class Servlet extends ScalatraServlet with ScalateSupport {
	// Instantiate database connection through singleton construction
	// Without this Squeryl will fail to perform database operations
	Database.getClass()
	import Servlet._
	def dump(title:String, data:String) = {
		contentType = "text/html"
		layoutTemplate("dummy", "title"->title, "body"->data)
	}
	val consoleWriter = new java.io.OutputStreamWriter(java.lang.System.out, "cp866")
	def printConsole(data: String) {
		consoleWriter.write(data+"\n")
		consoleWriter.flush
	}
	def error(message:String, reason:RuntimeException = null) = {
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
	def paramChecked(name:String) = {
		val valStr = params.getOrElse(name, "off")
		valStr=="on" || valStr=="ON"
	}
	def deleteRequestEntries[K, T<:KeyedEntity[K]](prefix:String, table:Table[T], allowed:Iterable[T]) {
		for (s <- allowed) {
			if (paramChecked(prefix+s.id))
				table.delete(s.id)
		}
	}
	def groupsModifiableByCurrentUser:Set[WorkGroup] = {	
		//TODO: real group access control
		Schema.groups.toSet
	}
	def updateAssociations[O<:KeyedEntity[_], A<:KeyedEntity[_]](prefix:String, allowed:Set[O], association:ManyToMany[O,A]) {
		val old:Set[O] = association.toSet
		for (o <- allowed) {
			if (paramChecked(prefix+o.id)) {
				if (!old.contains(o)) 
					association.associate(o)
			} else {
				association.dissociate(o)
			}
		}
	}
	def deleteAssociations[O<:KeyedEntity[_], A<:KeyedEntity[_]](prefix:String, allowed:Set[O], association:ManyToMany[O,A]) {
		val old:Set[O] = association.toSet
		for (o <- allowed) {
			if (paramChecked(prefix+o.id)) {
				association.dissociate(o)
			}
		}
	}

	// Redirects if transaction returns None
	// Returns transaction result otherwise
	def transactionOrRedirect[A](target:String) (body: =>Option[A]) = {
		val rv = transaction {
			body
		}.asInstanceOf[Option[A]]
		rv.getOrElse(redirect(target))
	}
	
	def handleExceptions[T](body: => T) = {
		try {
			body
		} catch {
			case _:NotFound => resourceNotFound()
			case e:BadInput => error(e.getMessage, e)
			case e:Denied => error(e.getMessage, e)
		}
	} 
}

object Servlet {
	class NotFound(message:String = null, reason:Throwable = null) extends RuntimeException(message, reason)
	class BadInput(message:String = null, reason:Throwable = null) extends RuntimeException(message, reason)
	class Denied(message:String = null, reason:Throwable = null) extends RuntimeException(message, reason)
}
