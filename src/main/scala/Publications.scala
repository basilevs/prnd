package prnd;
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import java.util.Calendar

class Publications extends Servlet {
	def getPublication: Publication = {
		val yearStr = params.getOrElse("year",  (Calendar.getInstance.get(Calendar.YEAR)-1).toString)
		val year = if (yearStr=="") 0 else yearStr.toInt
		val publisherId = params.getOrElse("publisherId", "0").toInt
		val defaultPubType = Schema.publishers.lookup(publisherId).map(_.allowedPublications.head).getOrElse(PublicationType.Article)
		val rv = new Publication(
			publisherId,
			params.getOrElse("authorCount", "100").toInt,
			year,
			params.getOrElse("title", ""),
			PublicationType(params.getOrElse("pubType", defaultPubType.id.toString).toInt)
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
				layoutTemplate("publicationById",
					"it"->it,
					"authors" -> it.authors
				)
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
				layoutTemplate("publicationEdit", "it"->it, "authors" -> authorFlags, "allowedGroups" -> Schema.groups.toSet)
			} getOrElse
			resourceNotFound()
		}
	}
	class Invalid extends RuntimeException
	get("/:id/save") {
		var id:Int = params.getOrElse("id", "0").toInt
		try {
			transaction {
				val newData = getPublication
				val it = Schema.publications.lookup(id).getOrElse(newData)
				it.title = newData.title
				it.authorCount = newData.authorCount
				it.publisherId = newData.publisherId
				it.pubType = newData.pubType
				if (!it.isValid)
					throw new Invalid()
				Schema.publications.insertOrUpdate(it)
				assert(it.id != 0)
				id = it.id
				updateAssociations("a_", authorsModifiableByCurrentUser, it.authors)
				updateAssociations("g_", groupsModifiableByCurrentUser, it.groups)
			}
			redirect("../"+id) //Should be outside transaction!!!
		} catch {
			case e: NumberFormatException => error("Неправильно введены числа. Проверьте год издания и число авторов.", e)
			case e: Invalid => error("Проверьте, что тип публикации соответствует изданию, год, число авторов и заголовок введены верно.")
		}
	}
}
