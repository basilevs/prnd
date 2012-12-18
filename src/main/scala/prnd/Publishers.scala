package prnd;
import org.scalatra.UrlGenerator.url
import java.net.URL
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session

class Publishers extends Servlet {
	get("/") {
		transaction {
			val publishers:Iterable[Publisher] = Schema.publishers
			assert(publishers != null)
			contentType = "text/html"
			layoutTemplate("publishers", "publishers" -> publishers)
		}
	}
	get("/:id") {
		transaction {
			val publisher = Schema.publishers.lookup(getId)
			publisher.map { a =>
				contentType = "text/html"
				layoutTemplate("publisherById",
					"it" -> a
				)
			} getOrElse
			resourceNotFound()
		}
	}
	get("/:id/edit") {
		transaction {
			val publisher = Schema.publishers.lookup(getId).getOrElse(new Publisher("", 0))
			contentType = "text/html"
			layoutTemplate("publisherEdit",
				"it" -> publisher
			)
		}
	}

	def parsePublisher() = {
		new Publisher(
			params.get("name").get,
			params.get("cost").get.toFloat,
			paramChecked("isConference"),
			paramChecked("isInternational"),
			false
		)
	}
	get("/:id/save") {
		var id:Int = params.getOrElse("id", "0").toInt
		try {
			transaction {
				val newData = parsePublisher
				val it = Schema.publishers.lookup(id).getOrElse(newData)
				it.name = newData.name
				it.impact = newData.impact
				it.isConference = newData.isConference
				it.international = newData.international
				Schema.publishers.insertOrUpdate(it)
			}
			redirect("../"+id) //Should be outside transaction!!!
		} catch {
			case e: NumberFormatException => error("Неправильно введены числа. Цена издания - число с плавающей точкой.", e)
		}
	}
}
