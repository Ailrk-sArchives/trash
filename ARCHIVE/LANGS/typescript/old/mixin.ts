class Sprite {
  name = "";
  x = 0;
  y = 0;

  constructor (name: string) {
    this.name = name;
  }
}

// extends other class
type Constructor = new (...args: any[]) => {};


function Scale<TBase extends Constructor>(base: TBase) {
  return class Scaling extends base {
    _scale = 1;

    setScale(scale: number) {
      this._scale = scale;
    }

    get scale(): number {
      return this._scale;
    }
  }
}

const EightBitSprite = Scale(Sprite);
