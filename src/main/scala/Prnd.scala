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
	def authorsModifiableByCurrentUser:Set[Author] = {
		Schema.authors.toSet
	}
	get("/publicationById/:id/edit") {
		val id:Int = params.getOrElse("id", "0").toInt
		transaction {
			val publication = if (id == 0) Some(getPublication) else Schema.publications.lookup(id)
			publication.map{it =>
				contentType = "text/html"
				val authors:Set[Author] = authorsModifiableByCurrentUser++it.authors
				val authorFlags = authors.map( a => (a, it.authors.exists(_.id == a.id)))
				layoutTemplate("publicationEdit", "it"->it, "authors" -> authorFlags)
			} getOrElse
			resourceNotFound()
		}
	}
	get("/publicationById/:id/save") {
		var id:Int = params.getOrElse("id", "0").toInt
		transaction {
			val publication = Schema.publications.lookup(id).getOrElse(getPublication)
			val it = publication
			contentType = "text/html"
			it.title = params("title")
			val w = new java.io.OutputStreamWriter(java.lang.System.out, "cp866")
			w.write(it.title+"\n")
			w.flush
			it.authorCount = params("authorCount").toInt
//			print("ac:"+it.authorCount+"\n")
			Schema.publications.insertOrUpdate(it)
			assert(it.id != 0)
			id = it.id
			val oldAuthors:Set[Author] = it.authors.toSet
			for (a <-authorsModifiableByCurrentUser) {
				val fieldName = "a_"+a.id
				val valStr = params.getOrElse(fieldName, "off")
				if (valStr=="on" || valStr=="ON") {
					if (!oldAuthors.contains(a))
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
	def getPublication: Publication = {
		val rv = new Publication(
			params.getOrElse("publisherId", "0").toInt,
			params.getOrElse("authorCount", "100").toInt,
			params.getOrElse("year", "2012").toInt,
			params.getOrElse("title", "")
		)
		rv
	}
	def similarPublications(it:Publication) = {
		val fields:Iterable[String] = it.title.split(" +")
//		fields.map(println)
		def similarTitle(title:String) = {
			fields.size == 0 || !fields.exists(title.indexOf(_) == -1)
		}
		for (p <- Schema.publications
			if similarTitle(p.title)
			if (it.publisherId == 0 || it.publisherId == p.publisherId)
		) yield p
	}
	get("/publications") {
		transaction {
			contentType = "text/html"
			val it = getPublication
			layoutTemplate("publications", "it"->it, "publications" -> similarPublications(it))
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
