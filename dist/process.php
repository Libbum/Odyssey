<?php session_start();

if(!$_POST) exit;

if (!defined("PHP_EOL")) define("PHP_EOL", "\r\n");

$name           = $_POST['name'];
$email          = $_POST['email'];
$message        = htmlspecialchars($_POST['message']);
//sumbission data
$ipaddress = $_SERVER['REMOTE_ADDR'];
$dateTime = new DateTime('now', new DateTimeZone('Europe/Stockholm'));
$date = $dateTime->format('d/m/Y');
$time = $dateTime->format('H:i:s');

if (isset($_POST['verify'])) :
        $posted_verify   = $_POST['verify'];
        $posted_verify   = md5($posted_verify);
else :
        $posted_verify = '';
endif;

// Important Variables
$session_verify = $_SESSION['verify'];

if (empty($session_verify)) $session_verify = $_COOKIE['verify'];

if(!isEmail($email)) {
    echo '<font color="#962d3e">Error: You have entered an invalid e-mail address.</font>';
    exit();
} else if($session_verify != $posted_verify) {
    echo '<font color="#962d3e">Error: the verification code you entered is incorrect.</font>';
    exit();
}

if($error == '') {

         // Configuration options.
         //Send to address
         $address = "tim@neophilus.net";


         // Configuration option.
         // i.e. The standard subject will appear as, "You've been contacted by John Doe."

         // Example, $e_subject = '$name . ' has contacted you via Your Website.';

         $e_subject = 'Odyssey contact from ' . $name . '.';


         // Configuration option.
         // You can change this if you feel that you need to.
         // Developers, you may wish to add more fields to the form, in which case you must be sure to add them here.

         $e_body = "You have been contacted by $name through the Odyssey contact form." . PHP_EOL . PHP_EOL;
         $e_content = "<p><strong>Name: </strong> {$name} </p>
                       <p><strong>Email Address: </strong> {$email} </p>";
         $e_content .= "<p><strong>Message: </strong> {$message} </p>" . PHP_EOL . PHP_EOL;

         $e_footer = "<p>This message was sent from the IP Address: {$ipaddress} on {$date} at {$time}</p>";


         $msg = wordwrap($e_body . $e_content . $e_footer,70);

         $headers = "From: Neophilus.net <noreply@neophilus.net>" . PHP_EOL;
         $headers .= "Reply-To: $email" . PHP_EOL;
         $headers .= "MIME-Version: 1.0" . PHP_EOL;
         $headers .= "Content-type: text/html; charset=iso-8859-1" . PHP_EOL;


         if(mail($address, $e_subject, $msg, $headers)) {


         // Email has sent successfully, echo a success page.

         echo "<p>Thanks for your message <strong>$name</strong>.<br>I'll get back to you as soon as possible.</p>";

         } else {

         echo 'ERROR!';

         }

}

function isEmail($email) { // Email address verification, do not edit.

  return(filter_var($email, FILTER_VALIDATE_EMAIL));

}

?>
