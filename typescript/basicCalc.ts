// F bounded polymorphism.
// type of this is polymorphic corresponds to
// subtype of the containing class.

class BasicCalulator {
  public constructor(protected value: number = 0) {}

  public currentValue(): number {
    return this.value;
  }

  public add(operand: number): this { // this type allows F bounded polymorphic.
    this.value += operand;
    return this;
  }

  public multiply(operand: number): this {
    this.value *= operand;
    return this;
  }
}

// subtype of BasicCalculator
class ScientificCalculator extends BasicCalulator {
  public constructor(value = 0) {
    super(value);
  }

  public sin() {
    this.value = Math.sin(this.value);
    return this;
  }
}

export const v = new BasicCalculator(10)
  .multiply(10)
  .add(1)
  .currentValue();

export const s = new ScientificCalculator(10)
  .multiply(20)
  .sin()
  .currentValue();
