package prnd;
import java.net.URI
import java.io.File
import java.io.FileInputStream
import scala.xml._
import org.squeryl.PrimitiveTypeMode._
class Inspire(val author:Author, year:Int) {
	def tryJournal(name:String): Option[Publisher] = {
		from(Schema.publishers)(p => where (
				p.name === name
			)
			select(p)
		).headOption
	}
	def guessJournal(name:String, volume:String):Publisher = {
		tryJournal(name+volume(0)).orElse(tryJournal(name)).get
	}
	def processEndNote(data:Elem) {
		data \\ "pre" foreach { elem => 
			println(elem.text)
		}
	}
	//Use inside transaction
	def run {
		var uri = new URI("http", "inspirehep.net", "/search", "p=author:\""+author.inspireName+"\"+AND+date:\""+year+"\"&of=hx&rg=200","")
		println(uri)
		val stream = new FileInputStream(new File("search.htm"))
//		val stream = uri.toURL.openConnection.getInputStream
		processEndNote(XML.loadFile("search.htm"))
	}
}