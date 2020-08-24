<!DOCTYPE html>
<html lang="en>
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta charset="UTF-8">
    <title>fold</title>
    <link rel='shortcut icon' type='image/x-icon' href='favicon.ico' />
    <link rel="stylesheet" type="text/css" href="style.css">
  </head>
  <body>
    <div class="top">
        <div class="center">
          <a href="http://www.gregoryleeman.com/fold"><h1><img src='icon.png'>old</h1></a>
          <p style="margin:0; text-weight:bold;"><b>The hairpin bisulphite sequence analyser</b></p>
        </div>
    </div>
    <div class="middle">
      <div class="center">
        <?php
            if(isset($_POST['example'])){
                include 'example.php';
            }else{
                echo '<form method="POST">
                        Sequences (seperated by comma):
                        <textarea name="sqs" required></textarea><br>
                        <p>Hairpin:<input type="radio" name="side" value="left">Left<input type="radio" name="side" value="right" checked>Right</p>
                        <p>Batch sequence (before bisulphite conversion):<input type="text" name="batch" style="float: right;" required></p>
                        <p>Barcode length:<input type="number" name="barlen" style="float: right;" required></p>
                        <p>Sequence length (optional):<input type="number" name="seqlen" style="float: right;"></p>
                        <input type="Submit" name="submit" value="fold">
                      </form>
                ';
            }
        ?>
        <div class="divider"></div>
        <form method="POST">
          <input type="Submit" name="example" style="background: #898099;" value="example?"></input>
        </form>
        <div id="loader" class="loader"></div>
        <br><br>
      </div>
    </div>
    <div class="bottom">
      <?php

          include 'token.php';
					$db = new SQLite3('dat.db');

          $time = date("Y-m-d G:i:s", time() - (60*30));
          $sql = "SELECT `session_id` FROM `session` WHERE `session_time` < '".$time."'";
          $result = $db->query($sql);
          while($row = $result->fetchArray()){
              unlink("im/".$row[session_id].".png");
              unlink("txt/".$row[session_id].".txt");
          }


          $sql = "DELETE FROM `session` WHERE `session_time` < '".$time."'";
          $db->query($sql);
          
          if(isset($_COOKIE['sess'])){
              $sql = "SELECT * FROM `session` WHERE `session_id` = '".$_COOKIE[sess]."'";
              $result = $db->query($sql);
							if ($result->numColumns() && $result->columnType(0) != SQLITE3_NULL) {
                  $sess = $_COOKIE['sess'];
              }else{
                  setcookie('sess', '', time() - 1, "/");
              }
          }

          if(isset($_POST['submit'])){
              $sess = getToken(12);
              
              setcookie('sess', $sess, time() + (60 * 30), "/");
              
              $time = date("Y-m-d G:i:s", time());
              $sql = "INSERT INTO `session` VALUES ('".$sess."', '".$time."');";
              $db->query($sql);
              
              $exec = 
                  'Rscript run.R "'.
                  $_POST['sqs'].'" "'.
                  $_POST['batch'].'" '.
                  $_POST['barlen'].' "'.
                  $_POST['side'].'" "'.
                  $sess.'"'
              ;
              if(isset($_POST['seqlen'])){
                  $exec = $exec.' '.$_POST['seqlen'];
              }

              
              $r = fopen("temp.R", "w") or die("Unable to open file!");
              
              $txt = '
sqs <- "'.$_POST['sqs'].'
";
sqs <- lapply(strsplit(sqs, split=",")[[1]], function(x){
  return(casefold(str2chr(x), upper=TRUE))
  });
batch <- casefold(str2chr("'.$_POST['batch'].'"), upper=TRUE);
barlen <- as.integer('.$_POST['barlen'].');
side <- "'.$_POST['side'].'";
sess <- "'.$sess.'";
sqlen <- NA;';

              fwrite($r, $txt);
              fclose($r);

							$exec = $exec.' >> run.log 2>&1';

							exec($exec);
          }
          if(isset($sess)){

              echo "<img src='im/" . $sess . ".png', style='width:150mm;'>";
              $txt = "txt/" . $sess . ".txt";
              $out = fopen($txt, 'r');
              $linecount = 0;
              while(($line=fgets($out)) != false){
                $linecount = $linecount + 1;
              }
              $linecount = $linecount + 15;
              fclose($out);
              $out = fopen($txt, 'r');

              echo '<textarea class="scrollabletextbox" style="width:100%; white-space: pre; height:'.$linecount.'em;" rows=". $linecount ." wrap="off">';
              while(($line=fgets($out)) != false){
                  echo $line;
              }
              echo "</textarea>";
              fclose($out);
          }
      ?>
    </div>  
    <div class="foot">
      <div class="center">
        <p>&copy; 2018 GREGORY LEEMAN ALL RIGHTS RESERVED</p>
      </div>
    </div>
  </body>
</html>
