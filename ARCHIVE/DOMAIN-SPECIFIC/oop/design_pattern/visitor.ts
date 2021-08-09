// why this version is so much nicer.

interface Expr {
  kind_: string,
  accept<T>(visitor: Visitor<T>): T
}

interface Visitor<T> {
  visit(e: Lit): T;
  visit(e: Add): T;
}

class Add implements Expr {
  public kind_ = "Add";
  left: Expr;
  right: Expr;

  constructor(left: Expr, right: Expr) {
    this.left = left;
    this.right = right;
  }

  static check(e: Expr): e is Add {
    return e.kind_ == "Add";
  }

  public accept<T>(visitor: Visitor<T>): T {
    return visitor.visit(this);
  }
}

class Lit implements Expr {
  public kind_: string = "Lit"
  value: number;

  constructor(value: number) {
    this.value = value;
  }

  static check(e: Expr): e is Lit {
    return e.kind_ == "Lit";
  }

  public accept<T>(visitor: Visitor<T>): T {
    return visitor.visit(this);
  }
}

class Eval implements Visitor<number> {
  visit(e: Lit): number;
  visit(e: Add): number;
  visit(e: any) {
    if (Lit.check(e)) {
      return e.value;
    }

    if (Add.check(e)) {
      return e.left.accept(this) + e.right.accept(this);
    }
  }
}

class Dump implements Visitor<void> {
  visit(e: Lit): void;
  visit(e: Add): void;
  visit(e: any) {
    if (Lit.check(e)) {
      console.log(e);
    }

    if (Add.check(e)) {
      console.log(e);
    }
  }
}

function main() {
  let expr = new Add(new Lit(2),
    new Add(
      new Lit(3),
      new Lit(5)));
  let evaluator = new Eval();
  let v = expr.accept(evaluator);
  console.log("result: ", v);

  let dumper = new Dump();
  expr.accept(dumper);
}

main();
