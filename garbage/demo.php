<html>
<?php
  print("hello");
  echo "World\n";
  $boolean = true;
  $int1 = 3;
  unset($int1);

  $nowdoc = <<<'END'
  Multi line
  string
  END;

  $heredoc = <<<'END'
  Multi line
  string
  END;

  echo "Good" . "Bad" . "\n";

  // constant
  define("FOO", "something\n");
  echo FOO;

  // assoc and mutation.
  $assoc = array('One' => 1, 'Two' => 2, 'Three' => 3);
  $assoc ['One'] = 4;

  array_push($assoc, 'Five');
  $assoc['Five'] = 10;
  echo $assoc['Five'];
?>

<p><?= $assoc['Two'] ?></p>

</html>

<?php
  $x = 10;
  $y = 20;
  $x = $y;    // copy.
  $z = &$x;   // take ref to.

  var_dump($z);     // dump type and value of variable
  print_r($assoc);     // pretty print

?>


<?php
  if (true) {
    print "Good";
  }
  if (false) {
    print "Bad";
  }

  function myfun () {
    return "hello";
  }

  function add($x, $y = 1) {
    $result = $x + $y;
    return $result;
  }

  echo add(4);
  echo add(4, 3);
?>

<?php
  require("mod.php");
  $klass = new Klass("instance");
  echo "const from klass" . Klass::CONST;
  $subklass = new SubKlass("sub");
  $subklass->printme();
  $klass->printme();
?>

