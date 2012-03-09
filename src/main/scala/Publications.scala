package prnd;
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import java.util.Calendar

class Publications extends Servlet {
	def getPublication: Publication = {
		val yearStr = params.getOrElse("year",  (Calendar.getInstance.get(Calendar.YEAR)-1).toString)
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
//		Session.currentSession.setLogger(println(_))
		val publisher:Int = it.publisherId
		val year:Int = it.year
//		println("Similar publications "+publisher)
		val loaded:Iterable[Publication] = from(Schema.publications)(
			p => where(
					( publisher === 0 or p.publisherId === publisher ) and
					(year === 0 or p.year === year )
				)
			select(p)
		)
		for (p <- loaded if similarTitle(p.title) ) yield p
	}
	get("/") {
		transaction {
			contentType = "text/html"
			val it = getPublication
			layoutTemplate("publications", "it"->it, "publications" -> similarPublications(it))
		}
	}
	get("/:id") {
		val id:Int = params("id").toInt
		transaction {
			val publication = Schema.publications.lookup(id)
			publication.map { it =>
				contentType = "text/html"
				layoutTemplate("publicationById", "it"->it, "authors" -> Schema.publicationToAuthors.left(it))
			} getOrElse	resourceNotFound()
		}
	}
	def authorsModifiableByCurrentUser:Set[Author] = {
		Schema.authors.toSet
	}
	get("/:id/edit") {
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
	get("/:id/save") {
		var id:Int = params.getOrElse("id", "0").toInt
		try {
			transaction {
				val newData = getPublication
				val it = Schema.publications.lookup(id).getOrElse(newData)
				it.title = newData.title
				it.authorCount = newData.authorCount
				it.publisherId = newData.publisherId
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
			redirect("../"+id)
		} catch {
			case e: NumberFormatException => error("Неправильно введены числа. Проверьте год издания и число авторов.", e)
		}
	}
}