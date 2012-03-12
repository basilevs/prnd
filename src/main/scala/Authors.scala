package prnd;
import org.scalatra.UrlGenerator.url
import java.net.URL
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session

class Authors extends Servlet {
	get("/") {
		transaction {
			val authors:Iterable[Author] = Schema.authors
			assert(authors != null)
			contentType = "text/html"
			layoutTemplate("authors", "authors" -> authors)
		}
	}
	get("/:id") {
		transaction {
			val author = Schema.authors.lookup(getId)
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
	get("/:id/edit") {
		val id = getId
		transaction {
			val author = if (id == 0) Some(getAuthor) else Schema.authors.lookup(id)
			author.map { a =>
				contentType = "text/html"
				layoutTemplate("authorEdit", "it" -> a, "canChangeInspire" -> canChangeInspireName)
			} getOrElse
			resourceNotFound()
		}
	}
	get("/:id/saveName") {
		var id:Int = getId
		transaction {
			val name = params("name")
			val it = Schema.authors.lookup(id).getOrElse {new Author(name)}
			it.name = name
			Schema.authors.insertOrUpdate(it)
			assert(it.id != 0)
			id = it.id
		}
		redirect("edit")
	}
	get("/:id/saveInspire") {
		val id:Int = getId
		transaction {
			Schema.authors.lookup(id).map { author =>
				if (canChangeInspireName) {
					author.inspireName = params("inspireName")
					Schema.authors.update(author)
				}
			} getOrElse	resourceNotFound()
		}
		redirect("edit#inspire")
	}
	get("/:id/inspireImport") {
		val id:Int = getId
		transaction {
			Schema.authors.lookup(id).map { author =>
				new Inspire(author, params("year").toInt).run
			} getOrElse	resourceNotFound()
		}
		redirect("edit#publications")
	}
	
	get("/:id/addSubordinate") {
		var id:Int = getId
		try {
			transaction {
				Schema.authors.lookup(id).map { author =>
					author.subordinates.associate(
						new Subordinate(params("name"), SubordinateStatus(params("status").toInt), params("year").toInt, params("coLeadCount").toInt)
					)
				} getOrElse	resourceNotFound()
			}
			redirect("edit#subordinates")
		} catch {
			case e: NumberFormatException => error("Неправильно введен год или число соруководителей.", e)
		}
	}
	get("/:id/deleteSubordinates") {
		val id:Int = getId
		transaction {
			Schema.authors.lookup(id).map { author =>
				deleteRequestEntries[Int, Subordinate]("s_", Schema.subordinates, author.subordinates)
			} getOrElse resourceNotFound()
		}
		redirect("edit#subordinates")
	}
	get("/:id/deletePublications") {
		val id:Int = getId
		transaction {
			Schema.authors.lookup(id).map { author =>
				for (p <- author.publications) {
					val fieldName = "p_"+p.id
					val valStr = params.getOrElse(fieldName, "off")
					if (valStr=="on" || valStr=="ON") {
						author.publications.dissociate(p)
					}
				}
			} getOrElse resourceNotFound()
		}
		redirect("edit#publications")
	}
	get("/:id/addExtra") {
		val id:Int = getId
		try {
			transaction {
				Schema.authors.lookup(id).map { author =>
					author.extras.associate(
						new Extra(params("description"), params("year").toInt, params("cost").toFloat)
					)
				} getOrElse	resourceNotFound()
			}
			redirect("edit#extra")
		} catch {
			case e: NumberFormatException => error("Неправильно введен год или показатель.", e)
		}
	}
	get("/:id/deleteExtra") {
		val id:Int = getId
		transaction {
			Schema.authors.lookup(id).map { author =>
				deleteRequestEntries[Int, Extra]("e_", Schema.extra, author.extras)
			} getOrElse resourceNotFound()
		}
		redirect("edit#extra")
	}
}
