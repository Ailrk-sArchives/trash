# 2020-04-29
# path a override function into the result set
# really it is a decorator.


rec {
  makeOverridable = f: ogArgs:
  let ogRes = f ogArgs;
  in ogRes // { override = newArgs: makeOverridable f (ogArgs // newArgs);};
}
