<?php @session_start(); header("Content-Type: image/png");

$enc_num = rand(0, 9999);
$key_num = rand(0, 24);
$hash_string = substr(md5($enc_num), $key_num, 5); // Length of String
$hash_md5 = md5($hash_string);

$_SESSION['verify'] = $hash_md5;

// Fallback
setcookie("verify", $hash_md5, time()+3600, "/");

session_write_close();

// Verification Image Background Selection

$bgs = array("assets/verify/1.png","assets/verify/2.png","assets/verify/3.png");
$background = array_rand($bgs, 1);

// Verification Image Variables

$img_handle = imagecreatefrompng($bgs[$background]);
$text_colour = imagecolorallocate($img_handle, 130, 130, 130);
$font_size = 5;

$size_array = getimagesize($bgs[$background]);
$img_w = $size_array[0];
$img_h = $size_array[1];

$horiz = round(($img_w/2)-((strlen($hash_string)*imagefontwidth(5))/2), 1);
$vert = round(($img_h/2)-(imagefontheight($font_size)/2));

// Make the Verification Image

imagestring($img_handle, $font_size, $horiz, $vert, $hash_string, $text_colour);
imagepng($img_handle);

// Destroy the Image to keep Server Space

imagedestroy($img_handle);
