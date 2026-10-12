package spike.a

import scala.NamedTuple.AnyNamedTuple

// A module is a field whose type is a View of another named tuple, supplied with that tuple's layer
// marked by module(...): `db = module(DbModule.base(self.config.dbUrl))`. When the field is computed,
// Overridable.tie builds the layer focused through the field: its `self` is the root's final `db`,
// its `upper` the layer below's `db`, so late binding works through the whole tree and a deep
// override is a nested Override. The field's value is the module's lazy View.
//
// A BaseModule is complete (a base layer, which may replace a module in an override); a plain Module
// wraps an Override and needs the layer below. A base accepts only BaseModule.
class Module[S <: AnyNamedTuple] private[a] (val layer: Override[S]) {
  private[a] def tie(prefix: String, self: () => View[S], upper: () => View[S]): View[S] = layer match {
    case base: Overridable[S] => Overridable.tie(prefix, base.layers, Knot.Complete(base.base, self))
    case _ => Overridable.tie(prefix, layer.layers, Knot.Passing(self, upper))
  }
}

final class BaseModule[S <: AnyNamedTuple](layer: Overridable[S]) extends Module[S](layer)

// `using Self[?]` is only a scope guard: module(...) is usable inside a layer literal (Upper <: Self).
def module[S <: AnyNamedTuple](layer: Overridable[S])(using Self[?]): BaseModule[S] = new BaseModule(layer)

def module[S <: AnyNamedTuple](layer: Override[S])(using Self[?]): Module[S] = new Module(layer)
