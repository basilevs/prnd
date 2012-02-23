package prnd;

import org.squeryl.adapters.MySQLInnoDBAdapter
import org.squeryl.SessionFactory
import org.squeryl.{Session => SquerylSession}

//Initializes Squeryl
object Database {
	Class.forName("com.mysql.jdbc.Driver")
	def connection = java.sql.DriverManager.getConnection("jdbc:mysql://localhost:3306/squeryl?user=prnd")
	SessionFactory.concreteFactory = Some(() => SquerylSession.create(connection, new MySQLInnoDBAdapter))
}
