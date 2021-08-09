interface Bird {
  fly();
  layEggs();
}

interface Fish {
  swim();
  layEggs();
}


function getSmallPet(which: "fish" | "bird"): Fish | Bird {
  if (which === "bird") return {
    fly: () => { console.log("fly") },
    layEggs: () => { console.log("lay Eggs") },
  } as Bird;

  if (which === "fish") return {
    swim: () => { console.log("swim") },
    layEggs: () => { console.log("lay Eggs") },
  } as Fish;
}


function propertyCheck(pet: Fish | Bird) {
  if ((pet as Fish).swim) (pet as Fish).swim();
  else if ((pet as Bird).fly) ((pet as Bird).fly());
}

// user define type guard.

function isFish(pet: Fish | Bird): pet is Fish {
  return (pet as Fish).swim !== undefined;
}


