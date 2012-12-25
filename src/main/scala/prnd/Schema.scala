package prnd;
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Schema => SSchema, KeyedEntity, ForeignKeyDeclaration}
import org.squeryl.dsl.{CompositeKey2}
import org.squeryl.annotations.Column


object Schema extends SSchema {
	override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) = foreignKeyDeclaration.constrainReference
	val publishers = table[Publisher]
	on(publishers)(s => declare(
	    s.name      is(unique,indexed)
	))

	val publications = table[Publication]
	on(publications)(s => declare(
		s.title	is(dbType("varchar(255)"))
	))
	val publisherToPublications = manyToManyRelation[Publisher, Publication, PublisherToPublication](publishers, publications)
		.via[PublisherToPublication]((pr, pn, pp) => (pr.id === pp.publisherId, pn.id === pp.publicationId))
	publisherToPublications.leftForeignKeyDeclaration.constrainReference(onDelete cascade)
	publisherToPublications.rightForeignKeyDeclaration.constrainReference(onDelete cascade)
	
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
	
	val groups = table[WorkGroup]
	val authorToGroup = manyToManyRelation[Author, WorkGroup, AuthorToGroup](authors, groups)
		.via[AuthorToGroup]( (a, g, ag) => (a.id === ag.authorId, ag.groupId === g.id) )
	val publicationToGroup = manyToManyRelation[Publication, WorkGroup, PublicationToGroup](publications, groups)
		.via[PublicationToGroup]( (a, g, ag) => (a.id === ag.publicationId, ag.groupId === g.id) )

	def addPermanentEntries {
		
	}
	
	//To be executed inside transaction
	def addInitialEntries {
		val babar = groups.insert(new WorkGroup("BaBar"))
		val kedr  = groups.insert(new WorkGroup("KEDR"))
		val atlas  = groups.insert(new WorkGroup("ATLAS"))
		val aerogel = groups.insert(new WorkGroup("Aerogel"))
		def pr(name:String, cost:Float, inter:Boolean = true) = {
			publishers.insert(new Publisher(name, cost, false, inter, true))
		}
		val phRD = pr("Phys.Rev.D", 4.964F)
		pr("Phys.Rev.Lett.", 7.622F)
		pr("Nucl.Phys.Proc.Suppl.", 0)
		pr("Phys.Lett.B", 5.255F)
		pr("Nucl.Instrum.Meth.A", 1.142F)
		pr("arXiv", 6F)
		pr("Chin.Phys.C", 1.343F)
		pr("PoS", 0.2F)
		pr("INP", 6F, false)
		pr("Eur.Phys.J.C", 3.248F)
		pr("JHEP", 6.049F)
		pr("New J.Phys.", 4.2F)
		pr("Nucl.Phys.B", 4.642F);
		{
			val author  = authors.insert(new Author("Бобровников Виктор Сергеевич","V.S.Bobrovnikov.1"))
			author.groups.associate(kedr)
			author.groups.associate(atlas)
		}
		{
			val author  = authors.insert(new Author("Барняков Александр Юрьевич","Y.Y.Barnyakov.2"))
			author.groups.associate(kedr)
			author.groups.associate(aerogel)
		}
		{
			val author  = authors.insert(new Author("Барняков Михаил Юрьевич","Y.Y.Barnyakov.1"))
			author.groups.associate(kedr)
			author.groups.associate(aerogel)
		}
		{
			val author  = authors.insert(new Author("Басок Иван Юрьевич","Y.Y.Basok.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Блинов Александр Евгеньевич","A.E.Blinov.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Бузыкаев Алексей Рафаилович","A.R.Buzykaev.1"))
			author.groups.associate(kedr)
			author.groups.associate(aerogel)
		}
		{
			val author  = authors.insert(new Author("Гулевич Василий Викторович","V.V.Gulevich.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Гусев Дмитрий Владимирович","D.V.Gusev.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Кононов Сергей Анатольевич","S.A.Kononov.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Кравченко Евгений Анатольевич","E.A.Kravchenko.2"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Николаев Иван Борисович","I.B.Nikolaev.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Онучин Алексей Павлович","A.P.Onuchin.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Присекин Вячеслав Геннадьевич","V.G.Prisekin.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Сковпень Юрий Иванович","Y.I.Skovpen.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Таюрский Валерий Алексеевич","V.A.Tayursky.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Тельнов Валерий Иванович","V.I.Telnov.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Харламова Татьяна Александровна","T.A.Kharlamova.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Тодышев Корнелий Юрьевич","Y.Y.Todyshev.1"))
			author.groups.associate(kedr)
		}
		{
			val author  = authors.insert(new Author("Шамов Андрей Георгиевич","A.G.Shamov.1"))
			author.groups.associate(kedr)
		}
		{
			val blinov  = authors.insert(new Author("Блинов Владимир Евгеньевич","V.E. Blinov"))
			blinov.groups.associate(kedr)
		}
	}
}

class Author(var name:String, var inspireName:String = "") extends KeyedEntity[Int] {
	val id = 0
	lazy val publications = Schema.publicationToAuthors.right(this)
	lazy val subordinates = Schema.authorToSubordinates.left(this)
	lazy val extras = Schema.authorToExtra.left(this)
	lazy val groups = Schema.authorToGroup.left(this)
	def cost:Float = publications.map(_.cost).sum + subordinates.map(_.cost).sum + extras.map(_.cost).sum	
}

class Authorship extends KeyedEntity[CompositeKey2[Int,Int]] {
	val author = 0
	val publication = 0
	def id = compositeKey(author, publication)
}

trait Cost {
	def cost: Float
}

class Publisher(var name: String, var impact:Float, var isConference:Boolean = false, var international:Boolean = true, var isImported:Boolean = false) extends KeyedEntity[Int] with Cost {
	val id = 0
	def allowedPublications:Set[PublicationType.Type] = {
		import PublicationType._
		if (isConference) {
			Set(Invited, Oral, Stand)
		} else {
			Set(Article, Preprint)
		}
	}
	lazy val publications = Schema.publisherToPublications.left(this)
	def cost = {
		if (isConference)
			0
		else if (international)
			impact * 30
		else
			impact * 45
	}
}

object Publisher {
	def all = {
		from(Schema.publishers)(select(_))
	}
}

object PublicationType extends Enumeration {
	type Type = Value
	val Article = Value(1, "Статья")
	val Invited = Value(2, "Приглашенный доклад")
	val Oral = Value(3, "Устный доклад")
	val Stand = Value(4, "Стэндовый доклад")
	val Preprint = Value(5, "Препринт")
}

class PublisherToPublication(var pubType: PublicationType.Type) extends KeyedEntity[CompositeKey2[Int,Int]] {
	def this() = this(PublicationType.Article)
	val publisherId = 0
	val publicationId = 0
	def id = compositeKey(publisherId, publicationId)
	lazy val publisher:Publisher = Schema.publishers.lookup(publisherId).get 
	lazy val publication:Publication = Schema.publications.lookup(publicationId).get 
	lazy val cost = {	
		import PublicationType._
		pubType match {
			case Article => publisher.cost / (if (publication.authorCount<10) publication.authorCount else 10)
			case Invited => if (publisher.international) 45 else 30
			case Oral => if (publisher.international) 15 else 10
			case Stand => if (publisher.international) 5 else 3	
			case Preprint => 6F / (if (publication.authorCount<10) publication.authorCount else 10) 
		}
	}
}
 

class Publication(
		var authorCount:Int = 0,
		var year: Int = 0,
		var title: String =""
	)  extends KeyedEntity[Int] with Cost {
	def this() = this(0)
	val id = 0
	lazy val authors = Schema.publicationToAuthors.left(this)
	lazy val publishers = Schema.publisherToPublications.right(this)
	lazy val groups = Schema.publicationToGroup.left(this)
	def cost:Float = {
		publishers.associationMap.map { case (p, ass) => ass.cost}.sum
	}
	def findOrInsert: Publication = {
		assert(id == 0)
		from(Schema.publications) ( p => where(
//TODO:				(p.publisherId === publisherId) and
				(p.year === year) and
				(p.title === title)
			)
			select(p)
		).headOption.getOrElse(Schema.publications.insert(this))
	}
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
	def cost = {
		import SubordinateStatus._
		val tmp: Float = (status match {
			case Dissertant => 30
			case Diplomant => 10
			case Aspirant => 6
			case Magistrant => 3
			case DisOunsil => 20
		})
		tmp / coLeadCount
	}
	val authorId = 0
}

class Extra (var description:String, var year:Int, var cost:Float) extends KeyedEntity[Int] with Cost {
	val id = 0
	val authorId = 0
}

//Can't name this Group due to mysql syntax
class WorkGroup(val name:String) extends KeyedEntity[Int] {
	val id = 0
	lazy val authors = Schema.authorToGroup.right(this)
	lazy val publications = Schema.publicationToGroup.right(this)
}

class AuthorToGroup(val authorId:Int, val groupId:Int) extends KeyedEntity[CompositeKey2[Int,Int]] {
	def id = compositeKey(authorId, groupId)
}

class PublicationToGroup() extends KeyedEntity[CompositeKey2[Int,Int]] {
	val publicationId:Int = 0
	val groupId:Int = 0
	def id = compositeKey(publicationId, groupId)
}