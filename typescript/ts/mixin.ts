// Disposable Mixin
class Disposable {
  isDisposed: boolean;
  dispose() {
    this.isDisposed = true;
  }
}

// Activatable Mixin
class Activatable {
  isActivate: boolean;
  activate() {
    this.isActivate = true;
  }

  deactivate() {
    this.isActivate = false;
  }
}

const applyMixins = (derived: any, bases: Array<any>) => {
  bases.forEach(base => {
    Object.getOwnPropertyNames(base.prototype).forEach(name => {
      Object.defineProperty(
        derived.prototype,
        name,
        Object.getOwnPropertyDescriptor(base.prototype, name)
      )
    })
  })
};

// use declaration mergin to merge declaration of SmartObject interface with the class.
interface SmartObject extends Disposable, Activatable {}
class SmartObject {
  constructor() {
    setInterval(() => console.log(this.isActivate + " : " + this.isDisposed), 500);
  }
  interact() {
    this.activate();
  }
}

applyMixins(SmartObject, [Disposable, Activatable]);

