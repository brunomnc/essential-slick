// Import the Slick interface for H2:
import slick.jdbc.H2Profile.api._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Example extends App {

  // Case class representing a row in our table:
  final case class Message(
    sender:  String,
    content: String,
    id:      Long = 0L)

  // Helper method for creating test data:
  def freshTestData = Seq(
    Message("Dave", "Hello, HAL. Do you read me, HAL?"),
    Message("HAL",  "Affirmative, Dave. I read you."),
    Message("Dave", "Open the pod bay doors, HAL."),
    Message("HAL",  "I'm sorry, Dave. I'm afraid I can't do that."),
    Message("Dave", "What if I say 'Pretty please'?"),
    Message("HAL",  "I'm sorry, Dave. I'm afraid I can't do that.")
  )

  // Schema for the "message" table:
  final class MessageTable(tag: Tag)
      extends Table[Message](tag, "message") {

    def id      = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def sender  = column[String]("sender")
    def content = column[String]("content")

    def * = (sender, content, id).mapTo[Message]
  }

  // Base query for querying the messages table:
  lazy val messages = TableQuery[MessageTable]

  // An example query that selects a subset of messages:
  val halSays = messages.filter(_.sender === "HAL")

  // Create an in-memory H2 database;
  val db = Database.forConfig("chapter02")

  // Helper method for running a query in this example file:
  def exec[T](program: DBIO[T]): T = Await.result(db.run(program), 2.seconds)

  try {

    // Create the "messages" table:
    println("Creating database table")
    exec(messages.schema.create)

    // Create and insert the test data:
    println("\nInserting test data")
    exec(messages ++= freshTestData)

    // Run the test query and print the results:
    println("\nSelecting all message sender names:")
    exec( messages.map(_.sender).result ) foreach { println }

    println("\nSelecting only Pretty messages:")
    println(
      exec {
        messages.
          map(_.content).
          filter(_ like "%Pretty%").
          result
      }
    )

    // filterOpt
    def query(name: Option[String]) =
      messages.filterOpt(name)( (row, value) => row.sender === value )

    println("\nfilterOpt, example SQL:")
    println(" With a value: "+query(Some("Dave")).result.statements.mkString)
    println(" Without a value: "+query(None).result.statements.mkString)

    println("\n Number of messages sent:")
    val result = exec( messages.result )
    println(result.length)

    println("\n Find the message with ID 1:")
    exec( messages.filter(_.id === 1L).result ) foreach { println }

    println("\n Is HAL real?")
    val hasHAL = for { m <- messages if m.content like "%HAL%" } yield m
    println( exec( hasHAL.exists.result ) )

    println("\n Just content columns")
    val justOneColumn = for { m <- messages } yield m.content
    exec( justOneColumn.result ) foreach { println }

    println("\n First result")
    val firstHALMessage = for { m <- messages if m.sender === "HAL" } yield m
    println( exec( firstHALMessage.result.head ) )

    println("\n Then the rest")
    val restOfHALMessages = for { m <- messages if m.sender === "HAL" } yield m
    exec( restOfHALMessages.take(5).result ) foreach { println }

    println("\n The start of something")
    val startsWithMessage = for { m <- messages if m.content.startsWith("Open") } yield m
    exec( startsWithMessage.result ) foreach { println }

    println("\n Liking")
    val liking = for { m <- messages if m.content like "%do%"} yield m.content
    exec( liking.result ) foreach { println }

  } finally db.close
}
