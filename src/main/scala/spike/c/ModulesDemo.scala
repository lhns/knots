package spike.c

import cats.syntax.all.*

// Modules: a member whose type is a dependency trait of its own is defined with that trait's layer,
// marked by module(...) as the whole body of the member. A module takes what it needs from outside
// as (by-name) parameters, wired by the parent.
object ModulesDemo {
  case class AppConfig(dbUrl: String, port: Int)
  case class Transactor(url: String)
  case class UserRepo(transactor: Transactor)
  case class Routes(userRepo: UserRepo, port: Int)

  trait DbDeps(using Upper[DbDeps]) {
    def transactor: Transactor = inherited
    def userRepo: UserRepo = inherited
  }

  trait ServerDeps(using Upper[ServerDeps]) {
    def routes: Routes = inherited
  }

  trait MainDeps(using Upper[MainDeps]) {
    def config: AppConfig = inherited
    def db: DbDeps = inherited
    def server: ServerDeps = inherited
  }

  object DbModule {
    def base(url: => String): Overridable[DbDeps] = Overridable[DbDeps](new DbDeps {
      override lazy val transactor = Transactor(url)
      override lazy val userRepo = UserRepo(self.transactor)
    })
  }

  object ServerModule {
    def base(userRepo: => UserRepo, port: => Int): Overridable[ServerDeps] =
      Overridable[ServerDeps](new ServerDeps {
        override lazy val routes = Routes(userRepo, port)
      })
  }

  val app = Overridable[MainDeps](new MainDeps {
    override lazy val config = AppConfig("jdbc:prod", 8080)
    override lazy val db = module(DbModule.base(self.config.dbUrl))
    override lazy val server = module(ServerModule.base(self.db.userRepo, self.config.port))
  })

  def main(args: Array[String]): Unit = {
    println("app: " + Overridable.build(app).server.routes)
    println("config: " + Overridable.build(app, Override[MainDeps](new MainDeps {
      override lazy val config = AppConfig("jdbc:test", 9090)
    })).server.routes)
    // A deep override: only db.transactor changes; server.routes sees it through db.userRepo.
    val fakeDb = Override[MainDeps](new MainDeps {
      override lazy val db = module(Override[DbDeps](new DbDeps {
        override lazy val transactor = Transactor("fake(" + upper.transactor.url + ")")
      }))
    })
    println("deep override: " + Overridable.build(app, fakeDb).server.routes)
  }
}
