rec {
  makeOverridable = f: ogArgs:
  let ogRes = f ogArgs;
  in ogRes // { override = newArgs: makeOverridable f (ogArgs // newArgs);};
}
