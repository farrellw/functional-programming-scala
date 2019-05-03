package chapterOne

//ChapterOne contains no exercises
object ChapterOne extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}
