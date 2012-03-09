package prnd;
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Schema => SSchema, KeyedEntity, ForeignKeyDeclaration}
import org.squeryl.dsl.{CompositeKey2}
import org.squeryl.annotations.Column


object Schema extends SSchema {
	override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) = foreignKeyDeclaration.constrainReference
	val publishers = table[Publisher]
	val publications = table[Publication]
	on(publications)(s => declare(
		s.title	is(dbType("varchar(255)"))
	))
	val publisherToPublications = oneToManyRelation(publishers, publications)
		.via((pr, pn) => pn.publisherId === pr.id)
	publisherToPublications.foreignKeyDeclaration.constrainReference(onDelete cascade)
	
	val authors = table[Author]
	val publicationToAuthors = manyToManyRelation[Publication, Author, Authorship](publications, authors)
		.via[Authorship]( (p, a, pa) => (p.id === pa.publication, pa.author === a.id) )
	publicationToAuthors.leftForeignKeyDeclaration.constrainReference(onDelete cascade)
	publicationToAuthors.rightForeignKeyDeclaration.constrainReference(onDelete cascade)
	
	val subordinates = table[Subordinate]
	val authorToSubordinates = oneToManyRelation(authors, subordinates)
		.via((a, s) => a.id === s.authorId)
	authorToSubordinates.foreignKeyDeclaration.constrainReference(onDelete cascade)
	
	val extra = table[Extra]
	val authorToExtra = oneToManyRelation(authors, extra)
		.via((a,e) => a.id === e.authorId)
	authorToExtra.foreignKeyDeclaration.constrainReference(onDelete cascade)
	
	//To be executed inside transaction
	def addInitialEntries {
		val pr = publishers.insert(new Publisher("Phys.Rev.D", 4.964F))
		publishers.insert(new Publisher("Phys.Rev.Lett.", 7.622F))
		val a = authors.insert(new Author("Сковпень Юрий Иванович","Skovpen, Yu.I."))
		authors.insert(new Author("Блинов Владимир Евгеньевич","Blinov, V.E."))
		val pn1 = publications.insert(new Publication(pr.id, 100, 2011, "Measurement of partial branching fractions of inclusive charmless B meson decays to K+, K0, and pi+"))
		pn1.authors.associate(a)
		val pn2 = publications.insert(new Publication(pr.id, 100, 2011, "Measurements of branching fractions, polarizations, and direct CP-violation asymmetries in B+ -> rho0 K*+ and B+ -> f0(980)K*+ decays"))
		pn2.authors.associate(a)
	}
}

class Author(var name:String, var inspireName:String = "") extends KeyedEntity[Int] {
	val id = 0
	lazy val publications = Schema.publicationToAuthors.right(this)
	lazy val subordinates = Schema.authorToSubordinates.left(this)
	lazy val extras = Schema.authorToExtra.left(this)
}

class Authorship(val author:Int, val publication:Int) extends KeyedEntity[CompositeKey2[Int,Int]] {
	def this(a:Author, pn:Publication) = this(a.id, pn.id)
	def id = compositeKey(author, publication)
}

trait Cost {
	def cost: Float
}

class Publisher(val name: String, val cost:Float) extends KeyedEntity[Int] with Cost {
	val id = 0
}

object Publisher {
	def all = {
		from(Schema.publishers)(select(_))
	}
}
 
class Publication(var publisherId: Int = 0, var authorCount:Int = 0, var year: Int = 0, var title: String ="")  extends KeyedEntity[Int] with Cost {
	val id = 0
	lazy val authors = Schema.publicationToAuthors.left(this)
	lazy val publisher = Schema.publisherToPublications.right(this)
	def isValid = {
		publisherId != 0 && year != 0 && title.length >5
	}
	def cost = publisher.single.cost / (if (authorCount<10) authorCount else 10)
}

object ReportType extends Enumeration {
	type Type = Value
	val Invited = Value(2, "Приглашенный доклад")
	val Oral = Value(3, "Устный доклад")
	val Stand = Value(4, "Стэндовый доклад")
}

object SubordinateStatus extends Enumeration {
	type Type = Value
	val Dissertant = Value(1, "Защитивший кандидатскую или докторскую")
	val Diplomant = Value(2, "Защитивший диплом бакалавра специалиста или магистра")
	val Aspirant = Value(3, "Аспирант")
	val Magistrant = Value(4, "Магистрант")
	val DisOunsil = Value(5, "Диссертационный совет")
	
}

class Subordinate (val name:String, val status:SubordinateStatus.Type, val year:Int, val coLeadCount:Int) extends KeyedEntity[Int] with Cost {
	def this() = this("", SubordinateStatus.Magistrant, 0, 0)
	val id = 0
	def cost = 0F
	val authorId = 0
}

class Extra (var description:String, var year:Int, var cost:Float) extends KeyedEntity[Int] with Cost {
	val id = 0
	val authorId = 0
}
