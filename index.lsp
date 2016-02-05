<%
(set *title* "TJ Entertainment")
(set *dir* (directory-namestring *path*))
%>
<!DOCTYPE html>
<html>
<head>
<title><%=*title*%></title>
<style type="text/css">body {font-family: verdana; font-size: 10pt;}</style>
<meta charset="UTF-8">
</head>
<body>
<%
(defun write (content) (string-append *content* content))
%>
<h1><%=*title*%></h1>
<p>Welcome and enjoy~</p>
<a href="/<%=(url "browse")%>">home</a>
<ul>
<%
(set *kilo* 1024)
(set *mega* (* 1024 1024))
(set *giga* (* 1024 1024 1024))
(defun kilo (num) (/ num *kilo*))
(defun mega (num) (/ num *mega*))
(defun giga (num) (/ num *giga*))
(defun size (num) (if (> num *giga*) (giga num) (if (> num *mega*) (mega num) (if (> num *kilo*) (kilo num) num))))
(defun size-type (num) (if (> num *giga*) "GB" (if (> num *mega*) "MB" (if (> num *kilo*) "KB" "bytes"))))
(defun li (item) (format "<li>~a</li>" item))
(defun mark (m) (format "<span style=\"display:inline-block;width:15px;\">~a</span>" m))
(defun anchor-browse (item) (format "<a href=\"~a?path=~a/~a\">~a</a>" (url "browse") *dir* item item))
(defun anchor-file (item) (format "<a href=\"~a?path=~a/~a\">~a</a>" (url "file") *dir* item item))
(defun print-file (item) (format "~a~a (~:d ~a)" (mark "&nbsp;") (anchor-file (pathname-name item)) (size (file-length item)) (size-type (file-length item))))
(defun print-dir (item) (format "~a~a" (mark "D") (anchor-browse (pathname-name item))))
(defun filter (item) (not (or (eq (pathname-name item) "..") (eq (pathname-name item) "."))))
(dolist (x (dir *dir*)) (if (filter x) (write (li (if (filep x) (print-file x) (print-dir x)))) nil))
%>
</ul>
<p>Lisp powered.</p>
</body>
</html>
