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
	get("/:id/edit") {
		val id:Int = params.getOrElse("id", "0").toInt
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
		var id:Int = params("id").toInt
		transaction {
			val name = params("name")
			val it = Schema.authors.lookup(id).getOrElse {new Author(name)}
			it.name = name
			Schema.authors.insertOrUpdate(it)
			assert(it.id != 0)
			id = it.id
		}
		redirect("/authors/"+id+"/edit")
	}
	get("/:id/saveInspire") {
		var id:Int = params("id").toInt
		transaction {
			Schema.authors.lookup(id).map { author =>
				author.inspireName = params("name")
				Schema.authors.update(author)
				redirect("/authors/"+id+"/edit")
			} getOrElse	resourceNotFound()
		}
	}
	get("/:id/addSubordinate") {
		var id:Int = params("id").toInt
		try {
			transaction {
				Schema.authors.lookup(id).map { author =>
					author.subordinates.associate(
						new Subordinate(params("name"), params("year").toInt)
					)
					redirect("/authors/"+id+"/edit")
				} getOrElse	resourceNotFound()
			}
		} catch {
			case e: NumberFormatException => error("Неправильно введен год.", e)
		}
	}
	get("/:id/deleteSubordinates") {
		var id:Int = params("id").toInt
		transaction {
			Schema.authors.lookup(id).map { author =>
				for (s <- author.subordinates) {
					val fieldName = "s_"+s.id
					val valStr = params.getOrElse(fieldName, "off")
					if (valStr=="on" || valStr=="ON") {
						Schema.subordinates.delete(s.id)
					}
				}
				redirect("/authors/"+id+"/edit")
			} getOrElse resourceNotFound()
		}
	}
	get("/:id/deletePublications") {
		var id:Int = params("id").toInt
		transaction {
			Schema.authors.lookup(id).map { author =>
				for (p <- author.publications) {
					val fieldName = "p_"+p.id
					val valStr = params.getOrElse(fieldName, "off")
					if (valStr=="on" || valStr=="ON") {
						author.publications.dissociate(p)
					}
				}
				redirect("/authors/"+id+"/edit")
			} getOrElse resourceNotFound()
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
