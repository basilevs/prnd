 import org.squeryl.PrimitiveTypeMode._
 import org.squeryl.{Schema, KeyedEntity, ForeignKeyDeclaration}
 import org.squeryl.KeyedEntity
 import org.squeryl.dsl.CompositeKey2
 import org.squeryl.annotations.Column
 import org.squeryl.adapters.MySQLInnoDBAdapter

import org.squeryl.SessionFactory

import org.squeryl.Session

object PrndSession {
	Class.forName("com.mysql.jdbc.Driver");
	def connection = java.sql.DriverManager.getConnection("jdbc:mysql://localhost:3306/squeryl?user=prnd")
	var done = false
	def connect {
		if (!done) {
			done = true
			SessionFactory.concreteFactory = Some(() => Session.create(connection, new MySQLInnoDBAdapter))
		}
	}
}


class PrndSchema extends Schema {
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
}

class Author(name:String) extends KeyedEntity[Int] {
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


