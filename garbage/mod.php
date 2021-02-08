<?php

  class Klass {
    const CONST = 'value';
    static $staticvar = 'static';
    public $instanceProp;

    public function __construct($instanceProp) {
      $this->instanceProp = $instanceProp;
    }

    final function nonOverridable() {
      print $this->CONST;
    }

    public function printme() {
      print "print me from Klass\n";
    }


    public function __toString() {
      return $property;
    }

    public function __destruct() {
      print "destructing\n";
    }

  }

class SubKlass extends Klass {
  public function printme () {
    print "print me from SubKlass\n";
  }

}
?>
