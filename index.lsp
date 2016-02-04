<%
(set *title* "Welcome")
(set *dir* (directory-namestring *path*))
%>
<!DOCTYPE html>
<html>
<head>
<title><%=*title*%></title>
</head>
<body>
<%
(defun write (content) (string-append *content* content))
(write (format "<h1>Title : ~a</h1>" *title*))
%>
<p>This is sample lisp page.</p>
<p>Path: <%=*dir*%></p>
<ul>
<%
(defun li (item) (format "<li>~a</li>" item))
(defun mark (m) (format "<span style=\"display:inline-block;width:15px;\">~a</span>" m))
(defun anchor-browse (item) (format "<a href=\"~a?path=~a/~a\">~a</a>" (url "browse") *dir* item item))
(defun anchor-file (item) (format "<a href=\"~a?path=~a/~a\">~a</a>" (url "file") *dir* item item))
(defun print-file (item) (format "~a~a" (mark "&nbsp;") (anchor-file (pathname-name item))))
(defun print-dir (item) (format "~a~a" (mark "D") (anchor-browse (pathname-name item))))
(dolist (x (dir *dir*)) (write (li (if (filep x) (print-file x) (print-dir x)))))
%>
</ul>
<a href="/<%=(url "browse")%>">home</a>
</body>
</html>
