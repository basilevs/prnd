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
	def isAuthorEditable(a:Author) = {
		//TODO: real author access control
		false
	}
	get("/:id") {
		transaction {
			val author = Schema.authors.lookup(getId)
			author.map { a =>
				contentType = "text/html"
				layoutTemplate("authorById",
					"it" -> a,
					"editable" -> isAuthorEditable(a)
				)
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
			author.filter(isAuthorEditable).map { a =>
				contentType = "text/html"
				layoutTemplate("authorEdit",
					"it" -> a,
					"canChangeInspire" -> canChangeInspireName,
					"allowedGroups" -> groupsModifiableByCurrentUser
				)
			} getOrElse
			resourceNotFound()
		}
	}
	class AccessDenied extends RuntimeException
	get("/:id/saveName") {
		var id:Int = getId
		transaction {
			val name = params("name")
			val it = Schema.authors.lookup(id).getOrElse {new Author(name)}
			if (!isAuthorEditable(it))
				throw new AccessDenied
			it.name = name
			Schema.authors.insertOrUpdate(it)
			assert(it.id != 0)
			id = it.id
		}
		redirect("edit")
	}
	get("/:id/saveGroups") {
		val id:Int = getId
		transactionOrRedirect("edit") {
			Schema.authors.lookup(id).map { author =>
				if (!isAuthorEditable(author))
					throw new AccessDenied
				updateAssociations("g_", groupsModifiableByCurrentUser, author.groups)
				None
			} getOrElse Option(resourceNotFound())
		} 
	}
	get("/:id/saveInspire") {
		val id:Int = getId
		transactionOrRedirect("edit#inspire") {
			Schema.authors.lookup(id).map { author =>
				if (!isAuthorEditable(author))
					throw new AccessDenied
				if (canChangeInspireName) {
					author.inspireName = params("inspireName")
					Schema.authors.update(author)
				}
				None //To redirect
			} getOrElse	Option(resourceNotFound())
		}
	}
	get("/:id/inspireImport") {
		val id:Int = getId
		transactionOrRedirect("edit#publications") {
			Schema.authors.lookup(id).map { author =>
				if (!isAuthorEditable(author))
					throw new AccessDenied
				new Inspire(author, params("year").toInt).run
				None
			} getOrElse	Option(resourceNotFound())
		}
	}
	
	get("/:id/addSubordinate") {
		var id:Int = getId
		try {
			transactionOrRedirect("edit#subordinates") {
				Schema.authors.lookup(id).map { author =>
					if (!isAuthorEditable(author))
						throw new AccessDenied
					author.subordinates.associate(
						new Subordinate(params("name"), SubordinateStatus(params("status").toInt), params("year").toInt, params("coLeadCount").toInt)
					)
					None
				} getOrElse	Option(resourceNotFound())
			}
		} catch {
			case e: NumberFormatException => error("Неправильно введен год или число соруководителей.", e)
		}
	}
	get("/:id/deleteSubordinates") {
		val id:Int = getId
		transactionOrRedirect("edit#subordinates") {
			Schema.authors.lookup(id).map { author =>
				if (!isAuthorEditable(author))
					throw new AccessDenied
				deleteRequestEntries[Int, Subordinate]("s_", Schema.subordinates, author.subordinates)
				None
			} getOrElse Option(resourceNotFound())
		}
	}
	get("/:id/deletePublications") {
		val id:Int = getId
		transactionOrRedirect("edit#publications") {
			Schema.authors.lookup(id).map { author =>
				if (!isAuthorEditable(author))
					throw new AccessDenied
				for (p <- author.publications) {
					val fieldName = "p_"+p.id
					val valStr = params.getOrElse(fieldName, "off")
					if (valStr=="on" || valStr=="ON") {
						author.publications.dissociate(p)
					}
				}
				None
			} getOrElse Option(resourceNotFound())
		}
	}
	get("/:id/addExtra") {
		val id:Int = getId
		try {
			transactionOrRedirect("edit#extra") {
				Schema.authors.lookup(id).map { author =>
					if (!isAuthorEditable(author))
						throw new AccessDenied
					author.extras.associate(
						new Extra(params("description"), params("year").toInt, params("cost").toFloat)
					)
					None
				} getOrElse	Option(resourceNotFound())
			}
		} catch {
			case e: NumberFormatException => error("Неправильно введен год или показатель.", e)
		}
	}
	get("/:id/deleteExtra") {
		val id:Int = getId
		transactionOrRedirect("edit#extra") {
			Schema.authors.lookup(id).map { author =>
				if (!isAuthorEditable(author))
					throw new AccessDenied
				deleteRequestEntries[Int, Extra]("e_", Schema.extra, author.extras)
				None
			} getOrElse Option(resourceNotFound())
		}
	}
}
