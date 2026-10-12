package spike.b

import cats.syntax.all.*

// Modules: a member whose type is a dependency trait of its own is supplied with that trait's layer, marked by module(...).
// A module takes what it needs from outside as (by-name) parameters, wired by the parent.
object ModulesDemo {
  case class AppConfig(dbUrl: String, port: Int)
  case class Transactor(url: String)
  case class UserRepo(transactor: Transactor)
  case class Routes(userRepo: UserRepo, port: Int)

  trait DbDeps {
    def transactor: Transactor
    def userRepo: UserRepo
  }

  trait ServerDeps {
    def routes: Routes
  }

  trait MainDeps {
    def config: AppConfig
    def db: DbDeps
    def server: ServerDeps
  }

  object DbModule {
    def base(url: => String): Overridable[DbDeps] = Overridable[DbDeps](new {
      def transactor = Transactor(url)
      def userRepo = UserRepo(self.transactor)
    })
  }

  object ServerModule {
    def base(userRepo: => UserRepo, port: => Int): Overridable[ServerDeps] =
      Overridable[ServerDeps](new { def routes = Routes(userRepo, port) })
  }

  val app = Overridable[MainDeps](new {
    def config = AppConfig("jdbc:prod", 8080)
    def db = module(DbModule.base(self.config.dbUrl))
    def server = module(ServerModule.base(self.db.userRepo, self.config.port))
  })

  def main(args: Array[String]): Unit = {
    println("app: " + Overridable.build(app).server.routes)
    println("config: " + Overridable.build(app, Override[MainDeps](new { def config = AppConfig("jdbc:test", 9090) })).server.routes)
    // A deep override: only db.transactor changes; server.routes sees it through db.userRepo.
    val fakeDb = Override[MainDeps](new {
      def db = module(Override[DbDeps](new { def transactor = Transactor("fake(" + upper.transactor.url + ")") }))
    })
    println("deep override: " + Overridable.build(app, fakeDb).server.routes)
  }
}
