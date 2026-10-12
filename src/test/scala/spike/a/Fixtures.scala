package spike.a

import java.util.concurrent.atomic.AtomicInteger

// Shared by the suites: plain flat sets and the module tree of ModulesDemo with effect counters.
case class Dep(name: String)

type Deps = (dep1: Dep, dep2: Dep, dep3: Dep)
type XY = (x: Int, y: Int)

def deps: Overridable[Deps] = Overridable[Deps]((
  dep1 = Dep("base1"),
  dep2 = Dep("base2"),
  dep3 = Dep("base3(" + self.dep2.name + ")"),
))

case class AppConfig(dbUrl: String, port: Int)
case class Transactor(url: String)
case class UserRepo(tx: Transactor)
case class Routes(repo: UserRepo)

type DbDeps = (transactor: Transactor, userRepo: UserRepo)
type ServerDeps = (routes: Routes, port: Int)
type AppDeps = (config: AppConfig, db: View[DbDeps], server: View[ServerDeps])

// Counts how often a transactor is created, per app.
final class App {
  val transactors = new AtomicInteger

  def dbBase(url: => String): Overridable[DbDeps] = Overridable[DbDeps]((
    transactor = { transactors.incrementAndGet(); Transactor(url) },
    userRepo = UserRepo(self.transactor),
  ))

  def serverBase(userRepo: => UserRepo, port: => Int): Overridable[ServerDeps] =
    Overridable[ServerDeps]((routes = Routes(userRepo), port = port))

  val base: Overridable[AppDeps] = Overridable[AppDeps]((
    config = AppConfig("jdbc:prod", 8080),
    db = module(dbBase(self.config.dbUrl)),
    server = module(serverBase(self.db.userRepo, self.config.port)),
  ))
}

// A db module whose members depend on each other: any access is a cycle.
def cyclicDb: Overridable[DbDeps] = Overridable[DbDeps]((
  transactor = Transactor(self.userRepo.tx.url),
  userRepo = UserRepo(self.transactor),
))
