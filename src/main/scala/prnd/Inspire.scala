package prnd;
import java.net.URI
import java.io.File
import java.io.FileInputStream
import scala.xml._
import org.squeryl.PrimitiveTypeMode._
import org.xml.sax.InputSource

class UnknownJournal(name:String, volume:String = "")
extends RuntimeException("Незарегистрированное издание: "+name+", том: "+volume) {
}

class Inspire(val author:Author, year:Int) {
	def tryJournal(name:String): Option[Publisher] = {
		from(Schema.publishers)(p => where (
				p.name === name
			)
			select(p)
		).headOption
	}
	def guessJournal(name:String, volume:String):Publisher = {
		val j = tryJournal(name+volume(0)).orElse(tryJournal(name))
		if (j.isEmpty)
			throw new UnknownJournal(name, volume)
		j.get
	}
	//Use inside transaction
	def run: Seq[Exception] = {
		var names = author.inspireName.split(",")
		def processName(name:String) = {
			saveArticles(getBibtex(name))
		}
		names.flatMap(processName)
	}
	type PublicationPublishers = (Publication, Seq[Publisher], Exception)
	def saveArticles(publications:Iterable[PublicationPublishers]): Seq[Exception] = {
		{for(pair <- publications) yield {
			if (pair._3 != null) {
				pair._3
			} else {
				try {
					val p = pair._1
					val saved = p.findOrInsert
					saved.authors.dissociate(author)
					saved.authors.associate(author)
					for (pr <- pair._2) {
						try {
							saved.publishers.dissociate(pr)
							saved.publishers.associate(pr)
						} catch {
							case e: Exception => throw new RuntimeException("Failed to associate "+saved+" and "+pr, e)
						}
					}
					null
				} catch {
					case e: Exception => e
				}
			}
		}}.toSeq.filter(_!=null)
	}
	def getBibtex(name:String) = {
		var uri = new URI("http", "inspirehep.net", "/search", "p=author:\""+name.trim+"\"+AND+date:\""+year+"\"&of=hx&rg=200","")
		println(uri)
//		val stream = new FileInputStream(new File("search.htm"))
		val stream = uri.toURL.openConnection.getInputStream
		val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
		val parser = parserFactory.newSAXParser()
		val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
		val xml = adapter.loadXML(new InputSource(stream), parser)
		val nodes:NodeSeq = (xml \\ "pre")
		nodes.map(bibTexNodeToPublication).flatten
	}
	def fieldToPair(field:String) = {
		val pair = field.split("=", 2)
		if (pair.length < 2) 
			throw new RuntimeException("Can't split by = :"+field)
		val name = pair(0).trim();
		val value = pair(1).replaceAll("[\n ]+", " ").trim().replaceAll("^\"|\"$", "");
		(name, value)
	}
	def parseHTML(data:String) = {
		scala.xml.Text(data).text
	}
	def bibTexNodeToPublication(elem:Node):Iterable[PublicationPublishers] = {
		val sections = elem.text.split("\",\n +")
		val fields = (sections filter (_.length > 4) map fieldToPair).toMap
//		println(fields("SLACcitation"))
//		println(fields("journal")+fields("volume"))
		val year = fields("year").toInt
		val raw = fields("title")
//		println("Raw: "+raw)
		val parsed1 = parseHTML(raw)
//		println("Parsed1: "+parsed1)
		val stripped = parsed1.replaceAll("^\\{|\\}$", "")
//		println("Stripped: "+stripped)
		val title = stripped.replaceAll("&gt;", ">");
//		println(title+": " + year)
		val p = new Publication(100, year, title)
		try {
			val publisher = guessJournal(fields("journal"), fields("volume"))
			Seq((p, Seq(publisher), null))
		} catch {
			case e:Exception => Seq((p, Seq(), e))
		}
	}
}