import org.scalatra.ScalatraServlet
import java.net.URL
import org.scalatra.scalate.ScalateSupport
import org.squeryl.PrimitiveTypeMode.transaction

class PrndServlet extends ScalatraServlet with ScalateSupport {
  PrndSession.connect
  get("/") {
  }
  def dump(title:String, data:String) = {
    contentType = "text/html"
    layoutTemplate("dummy", "title"->title, "body"->data)
  }
	def schemaToString = {
		val schema = new StringBuffer
		(new PrndSchema).printDdl(x=>schema.append(x+"\n"))
		schema.toString
  	}
	get("/schema") {
		transaction {
			dump("Database schema", schemaToString)
 		}
  	}
	//To be executed inside transaction
	private def addInitialEntries(s:PrndSchema) {
		val pr = s.publishers.insert(new Publisher("Phys.Rev.D", 4.964F))
		val a = s.authors.insert(new Author("Skovpen, Yu.I."))
		val pn = s.publications.insert(new Publication(pr.id, PublicationType.Article, 100, 2011, "Measurement of partial branching fractions of inclusive charmless B meson decays to K+, K0, and pi+"))
		s.associate(a, pn)
	}
	get("/schemaCreate") {
		transaction {
			val schema = new PrndSchema
			schema.create
			addInitialEntries(schema)
			dump("Database schema", schemaToString)      
		}
	}
	get("/schemaDrop") {
		transaction {
			val schema = new PrndSchema
			schema.drop
			schema.create
			addInitialEntries(schema)
			dump("Database schema reset", schemaToString)      
		}
	}

  notFound {
    // Try to render a ScalateTemplate if no route matched
    findTemplate(requestPath) map { path =>
      contentType = "text/html"
      layoutTemplate(path)
    } orElse serveStaticResource() getOrElse resourceNotFound() 
  }
}
