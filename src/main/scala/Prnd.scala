package prnd;
import org.scalatra.ScalatraServlet
import org.scalatra.UrlGenerator.url
import java.net.URL
import org.scalatra.scalate.ScalateSupport
import org.squeryl.PrimitiveTypeMode._

class Servlet extends ScalatraServlet with ScalateSupport {
	// Instantiate database connection through singleton construction
	// Without this Squeryl will fail to perform database operations
	Database.getClass()
	def dump(title:String, data:String) = {
		contentType = "text/html"
		layoutTemplate("dummy", "title"->title, "body"->data)
	}
	def schemaToString = {
		val schema = new StringBuffer
		Schema.printDdl(x=>schema.append(x+"\n"))
		schema.toString
  	}
	get("/schema") {
		transaction {
			dump("Database schema", schemaToString)
 		}
  	}
	get("/schemaCreate") {
		transaction {
			Schema.create
			Schema.addInitialEntries
			dump("Database schema", schemaToString)      
		}
	}
	get("/schemaDrop") {
		transaction {
			Schema.drop
			Schema.create
			Schema.addInitialEntries
			dump("Database schema reset", schemaToString)      
		}
	}
	get("/authorById/:id") {
		val id:Int = params("id").toInt
		transaction {
			val author = Schema.authors.lookup(id)
			author.map { a =>
				contentType = "text/html"
				layoutTemplate("authorById", "it" -> a, "publications" -> Schema.publicationToAuthors.right(a))
			} getOrElse
			resourceNotFound()
		}
	}
	get("/publicationById/:id") {
		val id:Int = params("id").toInt
		transaction {
			val publication = Schema.publications.lookup(id)
			publication.map { it =>
				contentType = "text/html"
				layoutTemplate("publicationById", "it"->it, "authors" -> Schema.publicationToAuthors.left(it))
			} getOrElse
			resourceNotFound()
		}
	}
	def authorsModifiableByCurrentUser = {
		from(Schema.authors)(select(_))
	}
	get("/publicationById/:id/edit") {
		val id:Int = params("id").toInt
		transaction {
			val publication = Schema.publications.lookup(id)
			publication.map { it =>
				contentType = "text/html"
				val authors:Iterable[Author] = authorsModifiableByCurrentUser
				val authorFlags = authors.map( a => (a, it.authors.exists(_.id == a.id)))
				layoutTemplate("publicationEdit", "it"->it, "authors" -> authorFlags)
			} getOrElse
			resourceNotFound()
		}
	}
	get("/publicationById/:id/save") {
		val id:Int = params("id").toInt
		transaction {
			val publication = Schema.publications.lookup(id).getOrElse(
				new Publication(0, 0, 0, ""))
			val it = publication
			contentType = "text/html"
			it.title = params("title")
			it.authorCount = params("authorCount").toInt
			print("ac:"+it.authorCount+"\n")
			Schema.publications.insertOrUpdate(it)
			assert(it.id != 0)
			val authors:Iterable[Author] = authorsModifiableByCurrentUser
			for (a <-authors) {
				val fieldName = "a_"+a.id
				val valStr = params.getOrElse(fieldName, "off")
				if (valStr=="on" || valStr=="ON") {
//					org.squeryl.Session.currentSession.setLogger(println(_))
//					Schema.publicationToAuthors.insertOrUpdate(new Authorship(a, it))
					if (it.authors.where(ar => ar.id === a.id).headOption.isEmpty)
						it.authors.associate(a)
				} else {
					it.authors.dissociate(a)
				}
			}
		}
		redirect("/publicationById/"+id)
	}
	get("/authors") {
		transaction {
			val authors:Iterable[Author] = from(Schema.authors)(select(_))
			assert(authors != null)
			contentType = "text/html"
			layoutTemplate("authors", "authors" -> authors)
		}
	}
	get("/publications") {
		transaction {
			val publications = from(Schema.publications)(select(_))
			contentType = "text/html"
			layoutTemplate("publications", "publications" -> publications)
		}
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
}
