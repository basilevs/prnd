package prnd;
import org.scalatra.UrlGenerator.url
import java.net.URL
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session

class Groups extends Servlet {
	get("/:id") {
		transaction {
			Schema.groups.lookup(params("id").toInt).map{ group => 
				contentType = "text/html"
				layoutTemplate("group", "it" -> group)
			}.getOrElse(resourceNotFound)
		}
	}
}
