package prnd;
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session

class Root extends Servlet {
	def schemaToString = {
		val schema = new StringBuffer
		Schema.printDdl(x=>schema.append(x+"\n"))
		schema.toString
  	}
	get("/") {
		redirect("authors")
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
			if (!initParameter("allowSchemaDrop").get.toBoolean)
				throw new Servlet.Denied("Access forbidden.")
			Schema.drop
			Schema.create
			Schema.addInitialEntries
			dump("Database schema reset", schemaToString)      
		}
	}
}
