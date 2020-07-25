;;; cs-org-publish.el --- publish website            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords:

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

(require 'ox)
(require 'ox-html)
(require 'ox-publish)
(require 'cs-org-publish-utils)
(require 'cs-org-transfer)


;; --- setting up a custom org-publish backend ---


(cl-defstruct cs-relative-paths filepath relative-link-to-sitemap relative-link-to-index absolute-path-to-github-org-file
              relative-link-to-about-for-project
              project-title)

;; (defconst cs-relative-paths "global-value")

(defconst cur-rel-paths ;; (make-cs-relative-paths :filepath nil
                        ;;                         :relative-link-to-sitemap
                        ;;                         nil
                        ;;                         :relative-link-to-index
                        ;;                         nil
                        ;;                         :absolute-path-to-github-org-file
  ;;                         nil)
  nil
  )



(defun cs-org-html-publish-to-backend (plist filename pub-dir &optional backend)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  ;; (message (concat (prin1-to-string anotherone)))
  (org-publish-org-to backend filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))

(defun cs-org-publish-project (&optional project-root-dir)
  "Publish the whole project.
   TODO: - check that only those files that are linked from org documents
           (and are in the org subfolder or the assets subfolder) are published as attachments
           use/modify the publishing function for that
         - operate on latex export blocks (which have a special tag),
           run latex on them, convert them to svg (if they aren't more than 1 page in total)
           and embed them into the html with a set width.
           if you want them to be exported to html as they would be to latex
           - to do this, you could use sth similar to
             #+ATTR_BEAMER: :options [Lagrange]
             but invent your own ATTR, e.g.:
             #+ATTR_KLIN: :options try_convert_svg_html_export width:300px

           Interesting links:
           https://emacs.stackexchange.com/questions/45751/org-export-to-different-directory
           - check if there is some way of running latex blocks and exporting their output to html
             and exporing them to latex as-is"

  (interactive)

  (unless project-root-dir
    (setq project-root-dir (get-next-project-root (buffer-file-name)
                                                  t)))

  (let* (;; (some-variable "hey")
         (project-name "site")
         (project-component-doc-name (concat project-name "org"))
         (project-component-other-name (concat project-name "other"))
         (project-component-all (concat project-name "all"))
         (project-base-dir
          (if project-root-dir
              (get-projects-base-dir-from-root-dir project-root-dir)
            (helm-read-file-name "Publish: Select project's base dir:"
                                 :initial-input (get-projects-base-dir (buffer-file-name)))))
         (project-publish-dir
          (if project-root-dir
              (get-projects-publish-dir-from-root-dir project-root-dir)
            (helm-read-file-name "Select project's publishing buffer (for preview) dir:"
                                 :initial-input (get-projects-publish-dir (buffer-file-name))))
                              ;; publish it directly
                              ;; (get-publish-dir-from-git-root nil cs-my-public-website-root-dir)
                              ))

    (setq org-publish-project-alist
          `((,project-component-doc-name
             :base-directory ,project-base-dir
             :base-extension "org"
             :publishing-directory ,project-publish-dir
             :recursive t
             :publishing-function
             ((lambda (plist filename pub-dir)
                (let* ((cur-rel-paths
                        (make-cs-relative-paths :filepath filename
                                                :relative-link-to-sitemap
                                                (concat (file-relative-name ,project-base-dir
                                                                            (file-name-directory filename))
                                                        "sitemap.html")
                                                :relative-link-to-index
                                                (concat (file-relative-name ,project-base-dir
                                                                            (file-name-directory filename))
                                                        "index.html")
                                                :absolute-path-to-github-org-file
                                                (if (or (string-equal (file-name-nondirectory filename) "index.org")
                                                        (string-equal (file-name-nondirectory filename) "sitemap.org"))
                                                    (get-project-repo-url (get-project-repo-name ,project-root-dir))
                                                  (get-edit-on-github-link (get-project-repo-name ,project-root-dir)
                                                                           ,project-root-dir
                                                                           filename))
                                                :relative-link-to-about-for-project
                                                (concat (file-relative-name ,project-base-dir
                                                                            (file-name-directory filename))
                                                        "about.html")
                                                :project-title (get-project-title ,project-root-dir))))
                  (cs-org-html-publish-to-backend plist filename
                                                  pub-dir 'my-html)
                  (my-filter-sorround-equation-labels-with-braces (get-publish-target-filepath ,project-root-dir (expand-file-name filename))))))
             :auto-sitemap t
             :sitemap-title "Sitemap")
            (,project-component-other-name :base-directory ,project-base-dir
                                           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
                                           :publishing-directory ,project-publish-dir
                                           :recursive t
                                           :publishing-function org-publish-attachment)
            (,project-component-all
             :components (,project-component-doc-name ,project-component-other-name))))

    (org-publish-reset-cache)
    (org-publish-remove-all-timestamps)
    (org-publish project-component-all t nil)

    (org-open-file project-publish-dir)
    ;; (when (yes-or-no-p "Do you want to open the preview folder? ")
    ;;   (shell-command (concat "nautilus " (prin1-to-string project-publish-dir) " &") publish-buffer-name publish-buffer-name))
    ))


;;----- high-level, pushing to website functions -------

(defun publish-project-offline (&optional project-root-dir)
  (interactive)
  (unless project-root-dir
    (setq project-root-dir (get-next-project-root (cond
                                                   ((equal major-mode 'org-mode)
                                                    (buffer-file-name))
                                                    ;; I assume you're in dired
                                                   ((equal major-mode 'dired-mode)
                                                    default-directory)
                                                   ((t buffer-file-truename)))
                                                  t))
    (unless project-root-dir
      (user-error "Root git dir not found")))
  ;; project
  (cs-clean-project-publish-buffer project-root-dir (concat "Clean out " (get-project-repo-name project-root-dir) "'s html directory: "))

  ;; generate index and about for this project
  (get-index-as-org-file project-root-dir)
  (get-about-as-org-file project-root-dir)

  (cs-org-publish-project project-root-dir))

(defun publish-project-to-website-repo-offline (&optional project-root-dir)
  (interactive)
  (let* (project-name)
    (unless project-root-dir
      (setq project-root-dir (get-next-project-root
                              (cond
                               ((equal major-mode 'org-mode)
                                (buffer-file-name))
                               ((equal major-mode 'dired-mode)
                                default-directory)
                               (t (user-error "Not in dired or org-mode")))
                              t "select local project which you want to publish"))
      (unless project-root-dir
        (user-error "Root git dir not found")))

    (setq project-name (get-project-repo-name project-root-dir))

    ;; clean project
    (cs-clean-project-publish-buffer project-root-dir (concat "Clean out " project-name "'s html directory: "))

    ;; generate index and about for this project
    (get-index-as-org-file project-root-dir)
    (get-about-as-org-file project-root-dir)

    (cs-org-publish-project project-root-dir)

    ;; website
    (cs-website-project-clear-and-paste
     ;; project publish dir
     (get-publish-dir-from-git-root t
                                    (get-next-git-root) )
     ;; website's publish dir
     (get-publish-dir-from-git-root t cs-my-public-website-root-dir project-name)

     ;; subproject's name
     project-name)

    (org-open-file cs-my-public-website-root-dir)
    ;; (when (yes-or-no-p "Preview the website's local repo? ")
    ;;   (shell-command (concat "nautilus " cs-my-public-website-root-dir " &") publish-buffer-name publish-buffer-name))
    ))


(defun cs-website-project-clear-and-paste (&optional source-dir target-dir subproject-name)
  "The website project could in the future be composed of several projects.
But it can also just be a plain directory (with a .git and .gitignore)
in which the html of a project (with an index.html is pasted)"
  (interactive)
  (let* ()
    (unless source-dir
      (setq source-dir (helm-read-file-name "Select buffer publishing dir: "
                                            :initial-input (get-publish-dir-from-git-root t
                                                                                          (get-next-git-root)))))
    (unless target-dir
      (setq target-dir (helm-read-file-name "Select public publishing dir: "
                                            :initial-input (get-publish-dir-from-git-root t cs-my-public-website-root-dir))))

    ;; clean out the website's www directory (or subdirectory)
    (if (not (file-exists-p target-dir))
        (if (yes-or-no-p (concat "Target dir " (prin1-to-string target-dir) " doesn't exist. Create it?"))
            (make-directory (file-name-as-directory target-dir) t)
          (user-error "Target dir not created, thus no target to write to clean out or to write to")))

    (cs-clean-project-publish-buffer
     (get-next-project-root target-dir t "Select website repo: ")
     "Clean out website's html directory: "
     (file-name-as-directory subproject-name))

    ;; override to website's git repository directory
    (with-output-to-temp-buffer publish-buffer-name
      (shell-command (read-shell-command "Copy the html to the website repo: "
                                         (concat "cp -af "
                                                 (prin1-to-string (concat (file-name-as-directory source-dir)
                                                                          "."))
                                                 " "
                                                 (prin1-to-string target-dir)))
                     publish-buffer-name))
    ;; (pop-to-buffer publish-buffer-name)
    ))

(defun cs-deploy-website-with-git ()
  (interactive)
  "Push the website's generated html to github pages."
  (let* ((big-project-root-dir cs-my-public-website-root-dir))
    (with-output-to-temp-buffer publish-buffer-name
      (shell-command (read-shell-command "Run the pushing command like this: "
                                         (concat " cd " (prin1-to-string big-project-root-dir) " ; "
                                                 " git add . ; git commit -m 'pushing html' ; git push ; "))
                     publish-buffer-name))

    (browse-url cs-my-github-website-url)))


;; ----- creating/updating the index.html of a project from it's set of org files ----

(defun get-index-as-org-file (&optional root-dir)
  "Publish the index.org (overview of recent posts) as an org file first.
Then, it can also be converted to an html file, together with
all other org files."
  (interactive)
  (with-temp-buffer
    (org-mode)
    (org-element-parse-buffer)
    (let* ((base-project-properties (parse-project-properties root-dir))
           (base-dir (get-org-dir-from-git-root (expand-file-name root-dir)))
           (pm-list (get-all-post-metadatas (expand-file-name base-dir)))
           sorted-list)
      (insert "#+OPTIONS: tex:dvisvgm\n")
      (insert "#+OPTIONS: num:nil\n")
      (insert "#+TITLE: "
              (get-project-title root-dir)
              "\n")
      ;; sort after last posted date and print
      (setq sorted-list (reverse (remove nil
                                         (mapcar (lambda (metadata)
                                                   (when (post-metadata-date metadata)
                                                     metadata))
                                                 (my-sort-for-what (copy-list pm-list)
                                                                   'post-metadata-date)))))
      (when sorted-list
        (insert "#+BEGIN_EXPORT html\n")
        ;; sort
        (let* ((ctr 0))
          (while (and (nth ctr sorted-list)
                      (< ctr 10))
            (print-post-metadata-into-org (nth ctr sorted-list)
                                          root-dir)
            (setq ctr (+ 1 ctr))))
        (insert "#+END_EXPORT\n"))
      (write-file (helm-read-file-name "Write the index file to: "
                                       :initial-input (concat base-dir "index.org"))))))

(defun get-about-as-org-file (&optional root-dir)
  "Publish the index.org (overview of recent posts) as an org file first.
Then, it can also be converted to an html file, together with
all other org files."
  (interactive)
  (with-temp-buffer
    (org-mode)
    (org-element-parse-buffer)
    (insert "#+OPTIONS: tex:dvisvgm\n")
    (insert "#+OPTIONS: num:nil\n")
    (insert "#+TITLE: About\n")
    (let* ((base-dir (get-org-dir-from-git-root (expand-file-name root-dir)))
           (base-project-properties (parse-project-properties root-dir)))
      ;; (insert "* About")
      (insert "\n")
      (insert "\n"
              "#+BEGIN_EXPORT html"
              "\n"
              (let* ((description-html (project-properties-description-html base-project-properties)))
                (if description-html
                    description-html
                  (warn (concat "No description put for project " (get-project-repo-name root-dir)))
                  "Oops! This project repository doesn't seem to have a description. My bad."))
              "\n"
              "<br></br>"
              "<br></br>"
              "<hr></hr>"
              "Check out "
              "<a href=\"" cs-my-github-website-url "\">my other blogs</a>"
              "."
              "<br></br>"
              "For general and legal information, please look "
              "<a href=\"" cs-my-github-website-legal-link "\">here</a>"
              "."
              "\n"
              "#+END_EXPORT"
              "\n")

      (write-file (helm-read-file-name "Write the about file to: "
                                       :initial-input (concat base-dir "about.org"))))))


;; --- importing an org file and it's needed assets into a project from outside -----

(defun cs-org-integrate-into-project (&optional org-file-path)
  "This means to select certain links in the org file (first level links) and
back them up into an assets directory, at the same level as the copied
org file, but also to check if that org file already links
to resources inside a project. There, links are not copied, but are merely
adjusted in the org file."
  (interactive)
  (unless org-file-path
    (setq org-file-path (buffer-file-name)))
  (cs-transfer-single-org-file org-file-path t))

(defun cs-org-publish-run-hydra ()
  ""
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-cs-org-publish
                                      (:columns 1 :exit t)
                                      "cs-org-publish: options for publishing a blog post"
                                      ,(when (equal major-mode 'org-mode)
                                         `("t s o"
                                           (lambda ()
                                             (interactive)
                                             (cs-transfer-single-org-file))
                                           "transfer single org file"))
                                      ,(when (equal major-mode 'org-mode)
                                         `("p l f"
                                           (lambda ()
                                             (interactive)
                                             (pull-files-into-asset-dir))
                                           "pull linked files into asset directory"))
                                      ("p p o"
                                       (lambda ()
                                         (interactive)
                                         (publish-project-offline))
                                       "publish the project offline for preview")
                                      ("p w r"
                                       (lambda ()
                                         (interactive)
                                         (publish-project-to-website-repo-offline))
                                       "publish the project offline to website repo")
                                      ("g b i a f"
                                       (lambda ()
                                         (interactive)
                                         (generate-index-html-for-base-project)
                                         (generate-about-html-for-base-project)
                                         (generate-legal-html-for-base-project))
                                       "generate base index.html, about.html and legal.html files")
                                      ("d w g"
                                       (lambda ()
                                         (interactive)
                                         (cs-deploy-website-with-git))
                                       "deploy website with git")
                                      ("q" nil "cancel")))))))
    (hydra-cs-org-publish/body)
    (fmakunbound 'hydra-cs-org-publish/body)
    (setq hydra-cs-org-publish/body nil))

(define-key org-mode-map (kbd "C-M-, P") ; process
  'cs-org-publish-run-hydra)

(define-key dired-mode-map (kbd "C-M-, P") ; process
  'cs-org-publish-run-hydra)


(cl-defstruct project-properties description-html visibility subproject-paths title)
(defconst project-properties-keyword-list
  (list "description-html" "ranking" "visibility" "subproject-paths" "title"))

(defun get-project-properties-relative-file-path (project-root)
  (expand-file-name (concat (file-name-as-directory project-root) project-properties-filename)))

(defun get-current-line ()
  (buffer-substring-no-properties (progn (beginning-of-line)
                                           (point))
                                    (progn (end-of-line)
                                           (point))))

(defun search-forward-to-the-next-of-list ()
  ""
  (let* ((original-point (point))
         (end-char (car (-sort '<
                           (remove nil
                                   (mapcar (lambda (keyword)
                                             (save-excursion
                                               (re-search-forward keyword nil t)
                                               (when (> (point) original-point)
                                                 (point))))
                                           project-properties-keyword-list))))))
    (when end-char
      (goto-char end-char))))

(defun search-forward-to-next-of-list-or-end-of-file ()
  ""
  (unless (search-forward-to-the-next-of-list)
    (goto-char (point-max)))
  (point))

(defun get-keyword-section-content (keyword-str)
  ""
  (save-excursion
    ;; start at the beginning
    (goto-char (point-min))
    (re-search-forward keyword-str nil t)
    (when (not (equal (point) (point-min)))
      ;; it has found the keyword
      (let* ((point-searched-from (point))
             (point-searched-to (progn
                                  (save-excursion
                                    (search-forward-to-next-of-list-or-end-of-file)
                                    (when (not (equal (point) point-searched-from))
                                      (point))))))
        (when point-searched-to
          (buffer-substring-no-properties (save-excursion (goto-char point-searched-from)
                                                          (end-of-line)
                                                          (point))
                                          (save-excursion (goto-char point-searched-to)
                                                          (if (equal point-searched-to (point-max))
                                                              (end-of-line)
                                                            (beginning-of-line))
                                                          (point))))))))

(defun parse-subproject-paths ()
  ""
  (mapcar (lambda (filepath)
            (file-name-as-directory filepath))
          (let* ((result (get-keyword-section-content "subproject-paths")))
            (when result
              (split-string-and-unquote result "\n")))))

(defun parse-visibility ()
  "Allowed: a simple `yes` or `no`.
That is after the linebreak of the keyword, of course.
If invalid value, assume `yes`.
If nothing specified, assume `yes`."
  (let* ((result
          (car (mapcar
                (lambda (str)
                  (string-trim str))
                (split-string-and-unquote
                 (concat (get-keyword-section-content "visibility"))
                 "\n")))))
    (cond
     ;; check for `no` string case-insensitively
     ((equalp result "no") nil)
     (t t))))

(defun parse-description-html ()
  ""
  (string-trim (concat (get-keyword-section-content "description-html"))))

(defun parse-title ()
  ""
  (string-trim (concat (get-keyword-section-content "title"))))

(defun parse-project-properties (project-root)
  ""
  (with-temp-buffer
    (insert-file-contents (get-project-properties-relative-file-path project-root))
    (make-project-properties :subproject-paths (parse-subproject-paths)
                             :description-html (parse-description-html)
                             :visibility (parse-visibility)
                             :title (parse-title))))

(defun generate-index-html-for-base-project (&optional project-root)
  "This base project has a directory structure:
"
  (unless project-root
    ;; (setq project-root
    ;;       (get-next-project-root (buffer-file-name)
    ;;                              t
    ;;                              "Select project root for which to generate an index.html: "))
    (setq project-root (expand-file-name cs-my-public-website-root-dir)))

  (let* ((base-project-properties (parse-project-properties project-root))
         (project-blocks-html
          ;; go through and collect the descriptions, then generate blocks
          ;; out of them and concatenate them into the html for the content
          (cl-reduce 'concat
                     (remove nil (mapcar* (lambda (pr subproject-name)
                                            (when (file-exists-p (concat project-root "www/" subproject-name "/index.html"))
                                              ;; only link to it if the index.html isn't a dead link
                                              (let* ((pp (parse-project-properties pr)))
                                                (cs-html-format-project-description-block
                                                 (project-properties-title base-project-properties)
                                                 ;; (project-properties-description-html pp)
                                                 (concat "./" ;; "www/"
                                                         subproject-name "/index.html")))))
                                          (project-properties-subproject-paths base-project-properties)
                                          (mapcar (lambda (root-dir)
                                                    (file-name-nondirectory (directory-file-name root-dir)))
                                                  (project-properties-subproject-paths base-project-properties)))))))
    (with-temp-buffer
      (insert (my-html-template-plain (cs-html-format-slidingtopbar-html nil cs-my-github-page-url
                                                                         "./about.html"
                                                                         "./legal.html")
                                      (cs-html-format-title-html (project-properties-description-html base-project-properties))
                                      (concat
                                       "<div class=\"project-container-container\">\n"
                                       project-blocks-html
                                       "</div>\n"
                                       )))
      (write-file (helm-read-file-name "Write the index file to: "
                                       :initial-input (concat (file-name-as-directory project-root)
                                                              "www/index.html"))))))

(defun generate-about-html-for-base-project (&optional project-root)
  "This base project has a directory structure:
"
  (unless project-root
    ;; (setq project-root
    ;;       (get-next-project-root (buffer-file-name)
    ;;                              t
    ;;                              "Select project root for which to generate an index.html: "))
    (setq project-root (expand-file-name cs-my-public-website-root-dir)))

  (let* ((base-project-properties (parse-project-properties project-root)))
    (with-temp-buffer
      (insert (my-html-template-plain nil
                                      ;; (cs-html-format-slidingtopbar-html nil
                                      ;;                                    nil nil)
                                      (cs-html-format-title-html
                                       "About")
                                      (project-properties-description-html base-project-properties)
                                      ))
      (write-file (helm-read-file-name "Write the about file to: "
                                       :initial-input (concat (file-name-as-directory project-root)
                                                              "www/about.html"))))))

(defun generate-legal-html-for-base-project (&optional project-root)
  ""
  (unless project-root
    ;; (setq project-root
    ;;       (get-next-project-root (buffer-file-name)
    ;;                              t
    ;;                              "Select project root for which to generate an index.html: "))
    (setq project-root (expand-file-name cs-my-public-website-root-dir)))

  (let* ((base-project-properties (parse-project-properties project-root)))
    (with-temp-buffer
      (insert (my-html-template-plain nil
                                      (cs-html-format-title-html
                                       "General and Legal Information")
                                      my-legal-html))
      (write-file (helm-read-file-name "Write the legal file to: "
                                       :initial-input (concat (file-name-as-directory project-root)
                                                              "www/legal.html"))))))

(defun cs-org-html-export-but-actually-publish (&optional async subtreep visible-only body-only ext-plist source-filepath target-filepath-dir)
  "Publish a single file with it's attachments.
Org-mode-export does not properly support export to another directory,
for example, it doesn't bring along the ltximg directory.
From inside an org file, export it to html.
If the selected directory doesn't exist however, and you
want to properly include your latex fragments as well (which doesn't work otherwise),
you need to actually /publish/ this one file to the directory."

  (let* (target-filepath
         source-filepath-dir
         target-filename)
    (unless source-filepath
      (setq source-filepath (buffer-file-name)))

    (setq source-filepath-dir (file-name-directory source-filepath))

    (setq target-filename (concat (file-name-base source-filepath)
                                  ".html"))

    (unless target-filepath-dir
      (setq target-filepath-dir
            ;; (file-name-directory "/home/chris/Desktop/mytest/org/other/testpost.html")
            (helm-read-file-name (concat "Choose directory for " target-filename
                                         " : ")
                                 :initial-input (concat (file-name-directory (buffer-file-name))))))

    (unless (file-exists-p target-filepath-dir)
      (make-directory target-filepath-dir t))

    (let* ((project-base-dir source-filepath-dir)
           (project-publish-dir target-filepath-dir)
           (org-publish-project-alist
            `(("document"
               :base-directory ,project-base-dir
               :base-extension "org"
               :publishing-directory ,target-filepath-dir
               :exclude ".*"
               :include [,source-filepath]
               :publishing-function ;; org-html-publish-to-html
               ((lambda (plist filename pub-dir)
                  (cs-org-html-publish-to-backend plist filename
                                                  pub-dir 'my-html))))
              ;; ("all"
              ;;  :components ("attachments" "document"))
              )))

      (org-publish-reset-cache)
      (org-publish-remove-all-timestamps)
      (org-publish "document" t nil)

      (setq target-filepath (concat target-filepath-dir target-filename))

      ;; run some processing commands on the produced html file
      (my-filter-sorround-equation-labels-with-braces target-filepath)

      ;; it first has to build all latex fragments
      ;; then it can include them
      ;; that's why there are two seperate publication processes

      (setq org-publish-project-alist
            `(("attachments" :base-directory
               ,project-base-dir
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
               ;; include web-stuff
               :publishing-directory ,project-publish-dir
               :recursive t
               :exclude ".*"
               :include ;; []
               ,(get-indispensable-assets-as-array source-filepath source-filepath-dir)
               :publishing-function org-publish-attachment)))

      (org-publish-reset-cache)
      (org-publish-remove-all-timestamps)
      (org-publish "attachments" t nil))

    (if (file-exists-p target-filepath)
        target-filepath
      (user-error (concat "File does not exist: "
                          (prin1-to-string target-filepath))))))

(defun get-indispensable-assets-as-array (source-filepath source-filepath-dir)
  "Get i.e. all embedded ltximg images."
  (let* ((ltximg-links (remove nil
                   (mapcar (lambda (filepath)
                             (when (string-match (file-name-base source-filepath) filepath)
                               filepath))
                           (directory-files (concat (file-name-as-directory (concat source-filepath-dir "ltximg")))
                                            t))))
         (embedded-web-stuff-links (remove nil
                                           (mapcar (lambda (filepath)
                                                     (if (not (file-exists-p filepath))
                                                         (warn (concat
                                                                "Linked-to file does not exist: "
                                                                (prin1-to-string filepath))))
                                                     filepath)
                                                   (cs-org-get-linked-files-more-types)))))
    (vconcat ltximg-links
             embedded-web-stuff-links)))



(provide 'cs-org-publish)
;;; cs-org-publish.el ends here
