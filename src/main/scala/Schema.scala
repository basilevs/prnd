package prnd;
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Schema => SSchema, KeyedEntity, ForeignKeyDeclaration}
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2
import org.squeryl.annotations.Column


object Schema extends SSchema {
	override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) = foreignKeyDeclaration.constrainReference
	val publishers = table[Publisher]
	val publications = table[Publication]
	val publisherToPublications = oneToManyRelation(publishers, publications)
		.via((pr, pn) => pn.publisherId === pr.id)
	val authors = table[Author]
	val publicationToAuthors = manyToManyRelation[Publication, Author, Authorship](publications, authors)
		.via[Authorship]( (p, a, pa) => (p.id === pa.publication, pa.author === a.id) )
	def associate(a:Author, pn:Publication) = {
		val ap = new Authorship(a, pn)
		publicationToAuthors.insert(ap)
		ap
	}
	publicationToAuthors.leftForeignKeyDeclaration.constrainReference(onDelete cascade)
	publicationToAuthors.rightForeignKeyDeclaration.constrainReference(onDelete cascade)
	//To be executed inside transaction
	def addInitialEntries {
		val s = this
		val pr = s.publishers.insert(new Publisher("Phys.Rev.D", 4.964F))
		val a = s.authors.insert(new Author("Skovpen, Yu.I."))
		val pn = s.publications.insert(new Publication(pr.id, PublicationType.Article, 100, 2011, "Measurement of partial branching fractions of inclusive charmless B meson decays to K+, K0, and pi+"))
		s.associate(a, pn)
	}
}

class Author(val name:String) extends KeyedEntity[Int] {
	val id = 0
}

class Authorship(val author:Int, val publication:Int) extends KeyedEntity[CompositeKey2[Int,Int]] {
	def this(a:Author, pn:Publication) = this(a.id, pn.id)
	def id = compositeKey(author, publication)
}

class Publisher(val name: String, val cost:Float) extends KeyedEntity[Int] {
	val id = 0
}
 
object PublicationType extends Enumeration {
	type PublicationType = Value
	val Invited = Value(1, "Invited")
	val Oral = Value(2, "Oral")
	val Stand = Value(3, "Stand")
	val Article = Value(4, "Article")
}

class Publication(val publisherId: Int, pubtype: PublicationType.PublicationType, val authorCount:Int, val year: Int, val title: String)  extends KeyedEntity[Int] {
	val id = 0
}


