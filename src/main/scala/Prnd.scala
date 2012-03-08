package prnd;
import org.scalatra.ScalatraServlet
import org.scalatra.UrlGenerator.url
import java.net.URL
import org.scalatra.scalate.ScalateSupport
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session

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
	def error(message:String, reason:RuntimeException) = {
		contentType = "text/html"
		layoutTemplate("error", "it" -> message, "error" -> reason)
	}
	get("/") {
		redirect("/authors")
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
	get("/authors") {
		transaction {
			val authors:Iterable[Author] = Schema.authors
			assert(authors != null)
			contentType = "text/html"
			layoutTemplate("authors", "authors" -> authors)
		}
	}
	get("/authors/:id") {
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
	def canChangeInspireName = false
	def getAuthor:Author = {
		new Author(
			params.getOrElse("name", ""),
			params.getOrElse("inspireName", "")
		)
	}
	get("/authors/:id/edit") {
		val id:Int = params.getOrElse("id", "0").toInt
		transaction {
			val author = if (id == 0) Some(getAuthor) else Schema.authors.lookup(id)
			author.map { a =>
				contentType = "text/html"
				println(a.name)
				layoutTemplate("authorEdit", "it" -> a, "publications" -> Schema.publicationToAuthors.right(a), "canChangeInspire" -> canChangeInspireName)
			} getOrElse
			resourceNotFound()
		}
	}
	get("/authors/:id/save") {
		var id:Int = params("id").toInt
		transaction {
			val newData = getAuthor
			val it = Schema.authors.lookup(id).getOrElse(newData)
			it.name = newData.name
			if (canChangeInspireName)
				it.inspireName = newData.inspireName
			Schema.authors.insertOrUpdate(it)
			assert(it.id != 0)
			id = it.id
		}
		redirect("/authors/"+id)
	}
	get("/publications") {
		transaction {
			contentType = "text/html"
			val it = getPublication
			layoutTemplate("publications", "it"->it, "publications" -> similarPublications(it))
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
		try {
			transaction {
				val newData = getPublication
				val it = Schema.publications.lookup(id).getOrElse(newData)
				it.title = newData.title
				it.authorCount = newData.authorCount
				it.publisherId = newData.publisherId
				val w = new java.io.OutputStreamWriter(java.lang.System.out, "cp866")
				w.write(it.title+"\n")
				w.flush
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
		} catch {
			case e: NumberFormatException => error("Неправильно введены числа. Проверьте год издания и число авторов.", e)
		}
	}
	def getPublication: Publication = {
		val yearStr = params.getOrElse("year", "")
		val year = if (yearStr=="") 0 else yearStr.toInt
		val rv = new Publication(
			params.getOrElse("publisherId", "0").toInt,
			params.getOrElse("authorCount", "100").toInt,
			year,
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
		Session.currentSession.setLogger(println(_))
		println("Similar publications")
		val publisher:Int = it.publisherId
		val year:Int = it.year
		val loaded:Iterable[Publication] = from(Schema.publications)(
			p => where(
					( publisher === 0 or p.id === publisher ) and
					(year === 0 or p.year === year )
				)
			select(p)
		)
		for (p <- loaded if similarTitle(p.title) ) yield p
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
