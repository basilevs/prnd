package prnd;
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Queryable, Query}
import org.squeryl.Session
import java.util.Calendar

class Publications extends Servlet {
	def authorsModifiableByCurrentUser:Set[Author] = {
		//TODO: real author access control
		Schema.authors.toSet
	}
	def isPublicationEditable(p:Publication) = {
		//TODO: real publication access control
		true
	}
	def getPublication: Publication = {
		val yearStr = params.getOrElse("year",  (Calendar.getInstance.get(Calendar.YEAR)-1).toString)
		val year = if (yearStr=="") 0 else yearStr.toInt
		val rv = new Publication(
			params.getOrElse("authorCount", "100").toInt,
			year,
			params.getOrElse("title", "")
		)
		rv
	}
	class BadPublisher(reason:Exception) extends RuntimeException(reason)
	def getPublisher: Option[Publisher] = {
		try {
			params.get("publisherId").map(_.toInt).map { id =>
				val publisher = Schema.publishers.lookup(id).get
				publisher
			}
		} catch {
			case e:NoSuchElementException => throw new BadPublisher(e)
			case e:NumberFormatException => throw new BadPublisher(e)
		}
	}
	def similarPublications(it:Publication, publishers:Set[Publisher]): Iterable[Publication] = {
		val fields:Iterable[String] = it.title.split(" +")
//		fields.map(println)
		def similarTitle(title:String) = {
			fields.size == 0 || !fields.exists(title.indexOf(_) == -1)
		}
		Session.currentSession.setLogger(println(_))
		val year:Int = it.year
//		println("Similar publications "+publisher)
		val query1:Queryable[Publication] = if (it.year == 0)
			Schema.publications
		else
			from(Schema.publications)( p =>
				where(
					(p.year === it.year )
				)
				select(p)
			)
		val query2:Queryable[Publication] = if (publishers.isEmpty) query1 else {
			join(query1, Schema.publisherToPublications) (
				(p,r) =>
					where(r.publisherId in publishers.map(_.id))
					select(p)
					on(r.publicationId === p.id)
			)
		}
		from(query2)(select(_)).filter(p => similarTitle(p.title))
	}
	get("/") {
		transaction {
			contentType = "text/html"
			val it = getPublication
			val publisher = getPublisher
			layoutTemplate("publications",
				"it"->it,
				"publications" -> similarPublications(it, publisher.toSet),
				"publisherId" -> publisher.map(_.id)
			)
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
					"authors" -> it.authors,
					"editable" -> isPublicationEditable(it)
				)
			} getOrElse	resourceNotFound()
		}
	}
	get("/:id/edit") {
		val id:Int = params.getOrElse("id", "0").toInt
		transaction {
			val publication = if (id == 0) Some(getPublication) else Schema.publications.lookup(id)
			publication.filter(isPublicationEditable).map {it =>
				contentType = "text/html"
				val authors:Set[Author] = authorsModifiableByCurrentUser++it.authors
				val authorFlags = authors.map( a => (a, it.authors.exists(_.id == a.id)))
				layoutTemplate("publicationEdit", "it"->it, "authors" -> authorFlags, "allowedGroups" -> groupsModifiableByCurrentUser)
			} getOrElse
			resourceNotFound()
		}
	}
	class Invalid(message:String) extends RuntimeException(message)
	get("/:id/save") {
		var id:Int = params.getOrElse("id", "0").toInt
		try {
			transaction {
				val newData = getPublication
				val it = Schema.publications.lookup(id).getOrElse(newData)
				it.title = newData.title
				it.authorCount = newData.authorCount
				it.year = newData.year
				if (it.title.length < 5)
					throw new Invalid("Title is too short")
				if (it.authorCount < 1)
					throw new Invalid("Wrong author count")
				if (it.year < 2010)
					throw new Invalid("Year is earlier then 2010")
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
