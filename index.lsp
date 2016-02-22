<%
(setq *title* "TJ Entertainment")
(setq *dir* (directory-namestring *path*))
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
<a href="/<%=(url "browse")%>">Home</a>
<ul>
<%
(setq *kilo* 1024)
(setq *mega* (* 1024 1024))
(setq *giga* (* 1024 1024 1024))
(defun kilo (num) (/ num *kilo*))
(defun mega (num) (/ num *mega*))
(defun giga (num) (/ num *giga*))
(defun size (num) (if (> num *giga*) (giga num) (if (> num *mega*) (mega num) (if (> num *kilo*) (kilo num) num))))
(defun size-type (num) (if (> num *giga*) "GB" (if (> num *mega*) "MB" (if (> num *kilo*) "KB" "Bytes"))))
(defun li (item) (format nil "<li>~a</li>" item))
(defun mark (m) (format nil "<span style=\"display:inline-block;width:15px;\">~a</span>" m))
(defun anchor-browse (item) (format nil "<a href=\"~a?path=~a/~a\">~a</a>" (url "browse") *dir* (url-encode item) item))
(defun anchor-file (item) (format nil "<a href=\"~a?path=~a/~a\">~a</a>" (url "file") *dir* (url-encode item) item))
(defun print-file (item) (format nil "~a~a (~:d ~a)" (mark "&nbsp;") (anchor-file (pathname-name item)) (size (file-length item)) (size-type (file-length item))))
(defun print-dir (item) (format nil "~a~a" (mark "D") (anchor-browse (pathname-name item))))
(defun filter (item) (not (or (eq (pathname-name item) "..") (eq (pathname-name item) ".") (eq (pathname-name item) "index.lsp"))))
(dolist (x (dir *dir*)) (if (filter x) (write (li (if (filep x) (print-file x) (print-dir x)))) nil))
%>
</ul>
<p>Lisp powered.</p>
</body>
</html>
