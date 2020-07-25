;;; cs-org-blog-html-backend.el --- custom html backend to export to blog  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(require 'cl-lib)
(require 'ox)
(require 'ox-publish)
(require 'ox-html)

(require 'cs-org-publish)

(require 'klin)  ;; for e.g. references (i.e. replacements of cite links by footnotes)

(org-export-define-derived-backend 'my-html 'html
  :translate-alist '((template . my-org-html-template))
  :menu-entry
  '(?H "Export to blog using my-html"
       ((?h "As HTML file" cs-org-html-simple-export)
        (?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (cs-org-html-export-but-actually-publish t s v b)
            (org-open-file (cs-org-html-export-but-actually-publish nil s v b)))))
        (?s "Do simple export (without publish)"
	    (lambda (a s v b)
	      (org-open-file (org-export-as 'my-html s v b)))))))

(defun cs-my-html-insert-get-org-head-content (info)
  ""
  (org-html--build-meta-info info)
  (org-html--build-head info)
  (org-html--build-mathjax-config info))

(defun cs-org-code-block-css-settings-hook (backend-used)
  "This sets all code blocks to not be colored, for a specific backend"
  (when (equal backend-used 'my-html)
    (setq org-html-htmlize-output-type nil)))

(add-hook 'org-export-before-processing-hook 'cs-org-code-block-css-settings-hook)

(defun get-my-html-preamble-str (&optional org-info no-org sliding-topbar-verbose-html)
  "This is a general html preamble string.
It can be called by org to publish an html page,
but also just from any function which wants to create an html file."
  (concat
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n"

"<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css\">"

   (unless no-org
     (cs-my-html-insert-get-org-head-content org-info))
   "\n
<style type=\"text/css\">
  body {
      font-family: 'Helvetica', 'Arial', sans-serif;
      margin: 0px;
  }

  #content {
      padding: 5px;
  }

  .slidingtopbar {
  }

@media screen and (min-width: 800px) {
	#content {
        margin: auto;
        width: 800px;
	}

    .slidingtopbar {
        margin: auto;
        width: 800px;
	}
}

/* 2: min-width */
@media screen and (min-width: 1000px) {
	#content {
        margin: auto;
        width: 1000px;
	}

    .slidingtopbar {
        margin: auto;
        width: 1000px;
	}
}

@media screen and (min-width: 1200px) {
	#content {
        margin: auto;
        width: 1200px;
	}

    .slidingtopbar {
        margin: auto;
        width: 1200px;
	}
}

.topbar {
  width: 100%;
  margin: 0;
  padding: 0;
  background-color: white;
  border-bottom:2px solid black;
  overflow: hidden;
  position: relative;
}

.aboutlink {
    border-bottom: 5px solid aquamarine;
    float: right;
}

.projectlink, .aboutlink {
  display: block;
  color: black;
  padding: 16px;
  text-decoration: none;
}

.projectlink {
    float: left;
}

.aboutlink {
    float: right;
}

.projectlink:hover:not(.active), .aboutlink:hover:not(.active){
  background-color: whitesmoke;
}

.projectlink {
    border-top: 5px solid cornflowerblue;
}

.posted, .lastedited{
color: #646769;
font-family: Arial,sans-serif;
font-size: .75em;
margin-right: 1em;
min-width: 80px;
display: inline-block;
}

.post-link {
    display: inline-block;
    align-self: bottom;
}

.post-container-div {
padding:5px;
transition: 0.3s;
}

.post-container-div:hover {
    /* padding-left: 10px; */
    background: whitesmoke;
}

@media screen and (max-width: 600px) {
.projectlink, .aboutlink {
    text-align: center;
    float: none;
    border:none;
  }

  .slidingtopbar {
      overflow: auto;
      width: auto;
  }

  .post-link {
    display: block;
    margin-bottom: 10px;
  }
}


pre.src {
width: auto;
overflow: visible;
font-family: Courier;
font-size: 10.5pt;
}

.org-src-container {
background-color: rgba(255, 255, 255, 0.9);
border-left: 2px solid black;
padding-left: 15px;padding-right: 15px;
color: black;
font-family: \"Courier New\", Courier, monospace;
}


.container{
    width: 100%;
    text-align: center;
}

.project-container-div{
    background: whitesmoke;
    width: 260px;
    padding-top: 10px;
    padding-bottom: 10px;
    padding-left: 50px;
    padding-right: 10px;
    margin-left: 10px;
    margin-right: 10px;
    display: inline-block;
    margin-bottom: 25px;
    margin-top: 25px;
    border-right: 5px solid cornflowerblue;
}

/* ---- prettier footnotes prettier ---- */

.footpara {
    width: 99%;
    text-align: left;
    padding: 0;
    margin: 0;
    display: inline;
}

.footdef {
    /* line-height: 3em; */
    padding-bottom: 30px;
}

.footdef > sup {
display:inline;
padding-right: 10px;
}


/* --- math display --- */

p > .org-svg {  /* export with dvisvgm: position adjustment of inline math */
    vertical-align: middle;
}

p > img {
    vertical-align: middle;
}

.org-svg { width: auto; } /* for export with dvisvgm */

.equation-container {
    display: table;
    text-align: center;
    width: 100%;
}

.equation-label {
    display: table-cell;
    text-align: right;
    vertical-align: middle;
}

p.footpara > a {
display: inline-flex;
}

</style>
</head>
<body>"
   (concat
    "
<div class=\"topbar\">
  <div class=\"slidingtopbar\">"
    (if sliding-topbar-verbose-html
        sliding-topbar-verbose-html
      (when cur-rel-paths
        (concat "<a " " class=\"projectlink\"" " href=\""
                (cs-relative-paths-relative-link-to-index cur-rel-paths)
                "\">" (cs-relative-paths-project-title cur-rel-paths) "</a>\n"
                "<a " " class=\"projectlink\" " " href=\""
                (cs-relative-paths-relative-link-to-sitemap cur-rel-paths)
                "\">Sitemap</a>\n"
                "<a " " class=\"projectlink\" " " href=\""
                (cs-relative-paths-absolute-path-to-github-org-file
                 cur-rel-paths) "\">Edit on Github</a>\n"
                "<a " " class=\"aboutlink\" " " href=\""
                (cs-relative-paths-relative-link-to-about-for-project
                 cur-rel-paths) "\">About</a>\n")))
    "</div>
</div>")))

(defun get-my-html-print-title-str (info)
  (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    ""))))))

(defun get-my-footer ()
  ""
  (concat "<footer><div class=\"slidingtopbar\"><span>Contact: 534ttl3@gmail.com</span></div></footer>"))

(defun get-my-html-postamble-str ()
  (concat
   ;; (get-my-footer)
   "
</body>
</html>"))

(defun my-org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."

  (interactive)
  (let* ()
    (concat
     (let ((link-up (org-trim (plist-get info :html-link-up)))
           (link-home (org-trim (plist-get info :html-link-home))))
       (unless (and (string= link-up "") (string= link-home ""))
         (format (plist-get info :html-home/up-format)
                 (or link-up link-home)
                 (or link-home link-up))))
     ;; Preamble.
     (org-html--build-pre/postamble 'preamble info)

     (get-my-html-preamble-str info)

     "\n<div id=\"content\">\n"
     ;; Print the title (if it's there)
     (get-my-html-print-title-str info)

     ;; Print the date (if it's there)
     (let* ((spec (org-html-format-spec info))
            (date (cdr (assq ?d spec))))
       (concat
        (and (plist-get info :with-date)
             (org-string-nw-p date)
             (format (concat "<span class=\"posted\">" ;; "%s: "
                             "%s</span>\n")
                     ;; (org-html--translate "" info)
                     date))
        ;; (and (plist-get info :time-stamp-file)
        ;;      (format
        ;;       "<span class=\"lastedited\">%s: %s</span>\n"
        ;;       (org-html--translate "last edited" info)
        ;;       (format-time-string
        ;;        (plist-get info :html-metadata-timestamp-format))))
        ))

     ;; (prin1-to-string cur-rel-paths)

     contents
     "\n</div>"
     ;; (org-html--build-pre/postamble 'postamble info)
     (get-my-html-postamble-str))))

(defun my-html-template-plain (&optional sliding-topbar-html title-html
                                         content-html)
  "Return the HTML of my template as a string."
  (concat (get-my-html-preamble-str nil t sliding-topbar-html)
          "\n<div id=\"content\">\n"
          title-html
          content-html
          "\n</div>"
          (get-my-html-postamble-str)))

(defun cs-org-html-replace-cites-by-footnotes (backend)
  ""
  (when (equal backend 'my-html)
      (let* ((cites-infos (klin-get-cites-infos)))
        (mapcar (lambda (cite-info)
                  (cs-org-replace-cite-by-footnote cite-info))
                ;; you have to reverse the order to
                ;; search from the bottom up.
                ;; otherwise, the positions of the found
                ;; cite links are changed every time
                ;; after replacing with a footnote
                (reverse cites-infos)))))

(defun cs-org-replace-cite-by-footnote (cite-info)
  "Replace a link of form [[cite:RockstuhlQMLehramt::2]] by an org footnote.
This is useful at html export."
  (let* ((cite-link-str (car cite-info))
         (kill-beg (nth 1 cite-info))
         (kill-end (nth 2 cite-info))
         ;; get the bibtex key from the cite string
         (bibtex-key (cdr (assoc 'bibtex-key (klin-get-assoc-list-from-cite-link-str cite-link-str))))
         (bibtex-filepath (expand-file-name (cdr (org-ref-get-bibtex-key-and-file bibtex-key))))
         (formatted-bibtex-entry-str (with-bib-file-buffer bibtex-filepath
                                                           (when (bibtex-search-entry bibtex-key)
                                                             (klin-bibtex-format-entry-to-string)))))
    (when formatted-bibtex-entry-str
      ;; replace link with footnote
      (save-excursion
        ;; delete link
        (kill-region kill-beg kill-end)
        ;; search up to beginning of cite link
        (goto-char kill-beg)
        (org-footnote-new)
        (insert " ")
        (insert formatted-bibtex-entry-str)))))


(defun cs-org-replace-link-types-by-nothing (backend link-type-str)
  "Replace a link type like bibliography:mybib.bib
by an empty string, but only for a certain export backend."
  (when (equal backend 'my-html)
      (let* ((links-infos (klin-get-links-infos link-type-str)))
        (mapcar (lambda (link-info)
                  (let* ((cite-link-str (car link-info))
                         (kill-beg (nth 1 link-info))
                         (kill-end (nth 2 link-info)))
                    (save-excursion
                      ;; delete link
                      (kill-region kill-beg kill-end))))
                ;; you have to reverse the order to
                ;; search from the bottom up.
                ;; otherwise, the positions of the found
                ;; cite links are changed every time
                ;; after replacing with a footnote
                (reverse links-infos)))))

(defun cs-org-replace-bibliography-link-by-nothing (backend)
  ""
  (cs-org-replace-link-types-by-nothing backend "bibliography"))


;; --------- filter to sorround equation labels with brackets ----------
;; I whish this kind of post-processing

;; "<span class=\"equation-label\">[[:space:]\n]+\\([0-9]+\\)[[:space:]\n]+</span>"

;; (let* ((my-regexp "a\\([0-9]+\\)b")
;;        (my-regexp-left "a")
;;        (my-regexp-right "b"))
;;   (replace-regexp-in-string my-regexp (lambda (x) (foo x my-regexp
;;                                                        my-regexp-left
;;                                                        my-regexp-right)) "a12b"))

(defun cs-org-braces-helper-function (x my-regexp my-str-left my-str-right)
  ""
  (string-match my-regexp x)
  (let* ((extracted-number-str (match-string 1 x)))
    (when extracted-number-str
      (concat my-str-left "(" extracted-number-str ")" my-str-right))))

(defun my-filter-sorround-equation-labels-with-braces (target-html-filepath)
  ""
  (with-temp-buffer
    (insert-file-contents target-html-filepath)
    (let* ((my-regexp "<span class=\"equation-label\">[[:space:]\n]+\\([0-9]+\\)[[:space:]\n]+</span>")
           (my-str-left "<span class=\"equation-label\">")
           (my-str-right "</span>")
           (transformed-str
            (replace-regexp-in-string my-regexp
                                      (lambda (x)
                                        (cs-org-braces-helper-function x my-regexp
                                                                       my-str-left my-str-right))
                                      (buffer-substring-no-properties (point-min)
                                                                      (point-max)))))
      (when transformed-str
        (kill-region (point-min) (point-max))
        (insert transformed-str)
        (write-file target-html-filepath)))))

;; (defun sorround-equation-labels-with-braces-get-string (html-string-original)
;;   ""
;;   (let* ((my-regexp "<span class=\"equation-label\">[[:space:]\n]+\\([0-9]+\\)[[:space:]\n]+</span>")
;;          (my-str-left "<span class=\"equation-label\">")
;;          (my-str-right "</span>")
;;          (transformed-str
;;           (replace-regexp-in-string my-regexp
;;                                     (lambda (x)
;;                                                                      my-str-left my-str-right))
;;                                     html-string-original)))))

;; (add-to-list 'org-export-filter-paragraph-functions
;;              'cs-org-dvisvgm-html-export-sorround-equation-labels-with-braces)


;; ---- hooks for processing org file before transcoding -----
(setq org-export-before-parsing-hook '())
(add-hook 'org-export-before-parsing-hook 'cs-org-html-replace-cites-by-footnotes)
(add-hook 'org-export-before-parsing-hook 'cs-org-replace-bibliography-link-by-nothing t)

(provide 'cs-org-blog-html-backend)
;;; cs-org-blog-html-backend.el ends here
