;;; cs-org-publish-utils.el --- Utils for cs-org-publish     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords: abbrev, abbrev, abbrev

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

;; ------- conventions ----------
;; This you might need to change depending on your installation
(defconst cs-my-public-website-root-dir (file-name-as-directory (expand-file-name "~/Dropbox/1Projects/programming/534ttl3.github.io")))
(defconst cs-my-github-page-url "https://github.com/534ttl3/")
(defconst cs-my-youtube-page-url "https://youtube.com/s41b0tproductions")
(defconst cs-my-github-website-repo-name "534ttl3.github.io")
(defconst cs-my-github-website-url (concat "https://" cs-my-github-website-repo-name "/"))
(defconst cs-my-github-website-about-link
  (concat cs-my-github-website-url "www/about.html"))
(defconst cs-my-github-website-legal-link
  (concat cs-my-github-website-url "www/legal.html"))
(defconst cs-my-github-website-repo-url (concat cs-my-github-page-url cs-my-github-website-repo-name))
(defconst project-properties-filename ".project-properties")
(defconst www-dir-name (file-name-as-directory "www"))
(defconst cs-org-publish-within-single-project-base-dir-name (file-name-as-directory "org"))
(defconst publish-buffer-name "*publish*")
(defconst cs-github-edit-master-subdir-path-name "./edit/master/")
(defconst cs-github-blob-master-subdir-path-name "./blob/master/")

(defconst my-legal-html (concat "<h3>Maintainer of this webpage</h3>"
                                "C. T. Schnur" "<br></br>" "2 Avenue Général Champon, 38000 Grenoble, France"
                                "<br></br>" "Email: " "534ttl3@gmail.com"
                                "<h3>Terms of Use</h3>" "This webpage is a personal blog and notebook. The maintainer does not guarantee for  accuracy of the content on this webpage. As everything that is just <i>out there</i> on the internet and not reviewed by third party professional editors, the contents of this page should always be taken with a grain of salt. "
                                "<h3>Privacy</h3>" "The webhost of this website, GitHub Pages, <i>may collect User Personal Information from visitors to your GitHub Pages website, including logs of visitor IP addresses, to comply with legal obligations, and to maintain the security and integrity of the Website and the Service.</i> (source: https://help.github.com/en/github/site-policy/github-privacy-statement ; accessed on the 6th of April 2020). "
                                "Apart from this data (which is not used or shared in any way by the above mentioned maintainer of this webpage), no other user data is collected, except if they themselves propose edits of the content on this page through GitHub."))

;; a link to a website could look like this:
;; https://github.com/534ttl3/derivations-site/blob/master/org/example.org

;; ------- getting relevant project directories or vcs directories automatically, or from file paths --------
(defun get-projects-base-dir (buffname)
  (expand-file-name (concat (file-name-as-directory (if (get-next-project-root buffname)
                                                        (get-next-project-root buffname)
                                                      (file-name-directory buffname)))
                            cs-org-publish-within-single-project-base-dir-name)))

(defun get-projects-publish-dir (buffname)
  (expand-file-name (concat (file-name-as-directory (if (get-next-project-root buffname)
                                                        (get-next-project-root buffname)
                                                      (file-name-directory buffname)))
                            www-dir-name)))

(defun get-projects-base-dir-from-root-dir (project-root-dir)
  (let* ((standard (concat project-root-dir cs-org-publish-within-single-project-base-dir-name)))
    (if (file-exists-p standard)
        standard
      (user-error (concat "Base dir " standard " does not exist")))))

(defun get-projects-publish-dir-from-root-dir (project-root-dir)
  (let* ((standard (concat project-root-dir www-dir-name)))
    standard
    ;; (if (file-exists-p standard)
    ;;     standard
    ;;   (user-error (concat "Publish for preview dir " standard " does not exist")))
    ))

(defun get-publish-target-filepath (project-root-dir source-filepath)
  "From an org file in the base dir, get the filepath of the html file in
the publish directory."
  (let* ((publish-dir (get-projects-publish-dir-from-root-dir project-root-dir))
         (base-dir (get-projects-base-dir-from-root-dir project-root-dir))
         (relative-part (file-name-directory (file-relative-name source-filepath base-dir)))
         (target-filepath (concat publish-dir relative-part (file-name-base source-filepath) ".html")))
    target-filepath))

(defun get-project-repo-name (project-root)
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory (file-name-as-directory project-root)))))

(defun get-project-repo-url (project-repo-name)
  (concat cs-my-github-page-url project-repo-name "/"))

(defun get-project-view-file-in-repo-url (project-repo-name)
  (concat cs-my-github-page-url project-repo-name "/"))

(defun get-edit-on-github-link (project-repo-name project-root-dir file-path)
  (concat cs-my-github-page-url (file-name-as-directory project-repo-name)
          cs-github-blob-master-subdir-path-name (file-relative-name file-path project-root-dir))
  )

(defun get-next-git-root (&optional ask prompt filepath)
  (let* ((automatically-found-dir (file-name-as-directory (car (split-string (shell-command-to-string
                                                                              (concat (when (and filepath (if (file-exists-p filepath)
                                                                                                              t
                                                                                                            (user-error (concat "Filepath " (prin1-to-string filepath) " given to get-next-git-root doesn't exist"))))
                                                                                        (concat " cd "
                                                                                                (prin1-to-string (expand-file-name (file-name-directory filepath)))
                                                                                                " ; "))
                                                                                      "git rev-parse --show-toplevel"))
                                                                             "\n")))))
    (if (not ask)
        (if automatically-found-dir
            automatically-found-dir
          (user-error "No git dir was automatically found"))
      (helm-read-file-name (if prompt
                               prompt
                             "Select git directory root: ")
                           :initial-input automatically-found-dir)))
  ;; TODO: give error or warning if it's not a git dir root
  )

(defun get-org-dir-from-git-root (&optional git-root)
  (unless (and git-root (file-exists-p git-root))
    (setq git-root (get-next-git-root)))
  (file-name-as-directory (concat git-root "org")))

(defun get-publish-dir-from-git-root (publish-to-buffer &optional git-root subproject-name)
  (unless (and git-root
               (file-exists-p git-root))
    (setq git-root (get-next-git-root)))
  (if publish-to-buffer
      (let* ((www-dir (file-name-as-directory (concat git-root www-dir-name))))
        (if subproject-name
            (file-name-as-directory (concat www-dir subproject-name))
          www-dir))
    ;; not publish to buffer means: publish index.html into the root, i.e. publish directly
    ;; FIXME: maybe remove this option eventually
    (file-name-as-directory git-root)))

(defun get-project-title (project-root-dir)
  "If it has a title, get the title, otherwise get the name of the repo."
  (let* ((base-project-properties (parse-project-properties project-root-dir)))
    (if (and (project-properties-title base-project-properties)
                       (not (string-equal
                             ""
                             (string-trim (project-properties-title base-project-properties)))))
                  (project-properties-title base-project-properties)
                (concat (get-project-repo-name project-root-dir)))))

(defun get-next-project-root (filepath &optional ask prompt)
  "From inside a path, get the project's root."
  (let* ((project-root-dir (get-next-git-root ask prompt filepath)))
    (when (and (file-exists-p (concat project-root-dir project-properties-filename))
               (file-exists-somewhere-within-folder-p filepath
                                                      project-root-dir))
      project-root-dir)))


;; ---- checking utils ------

(defun file-exists-somewhere-within-folder-p (file-path root-path)
  "check if file-path is in a subdirectory under root-path and not somewhere else."
  (let* ((rel-dir-path (file-relative-name file-path root-path)))
    (if (or (not (file-exists-p root-path))
            (not (file-exists-p file-path))
            (string-match-p (regexp-quote "..") rel-dir-path))
        nil
      rel-dir-path)))

(defun file-in-project-p (filepath)
  "Check if a file is in a project, i.e.:
  - the project's root directory is a git root
  - the project's root directory contains a file .project-properties"
  (let* ((project-root-dir (get-next-git-root)))
    (and (file-exists-p (concat project-root-dir project-properties-filename))
         (file-exists-somewhere-within-folder-p filepath
                                                project-root-dir))))

(defun files-under-same-project-p (file-path-1 file-path-2)
  "Check if files are under the same project, i.e.:
  - the project's root directory is a git root
  - the project's root directory contains a file .project-properties"
  ;; identify the current project's root directory
  (let* ((project-root-dir (get-next-git-root)))
    (and (file-exists-p (concat project-root-dir project-properties-filename))
         (file-exists-somewhere-within-folder-p file-path-1
                                                project-root-dir)
         (file-exists-somewhere-within-folder-p file-path-2
                                                project-root-dir))))


;; ---- metadata handling -------

(cl-defstruct post-metadata title date last-modified-date file-name-base relative-dir-path post-type)

(defun my-sort-for-what (mylist sort-for-what)
  (let* ()
    (sort mylist
          `(lambda (elem1 elem2)
             (string-lessp (,sort-for-what elem1)
                           (,sort-for-what elem2))))))

(defun get-all-post-metadatas (base-dir &optional forbidden-file-names)
  (interactive)
  (setq base-dir (expand-file-name base-dir))
  (setq forbidden-file-names (list "sitemap.org" "index.org" "index.org"))
  ;; (setq base-dir (expand-file-name "/home/chris/Dropbox/1Projects/programming/derivations-site"))
  (let* ((file-path-list (remove nil
                                 (mapcar (lambda (file-path)
                                           (when (not (member (file-name-nondirectory file-path) forbidden-file-names))
                                             file-path))
                                         (remove nil
                                                 (mapcar (lambda (file-path)
                                                           (when (and (not (string-equal file-path ""))
                                                                      (file-exists-p file-path))
                                                             file-path))
                                                         (split-string (shell-command-to-string (concat "find "
                                                                                                        (prin1-to-string base-dir)
                                                                                                        " -name \"*.org\" -type f"))
                                                                       "\n")))))))
    (remove nil
            (mapcar (lambda (path)
                      (let* ((metadata (parse-org-file-to-metadata path base-dir)))
                        (when (char-or-string-p (post-metadata-post-type metadata))
                          (when (not (string-equal ""
                                                   (post-metadata-post-type metadata)))
                            metadata))))
                    file-path-list))))

(defun org-global-props-get-plist (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun parse-org-file-to-metadata (&optional file-path base-dir)
  "Get metadata from an org file at FILE-PATH as a function of BASE-DIR."
  (interactive)
  (unless file-path
    ;; (setq file-path (expand-file-name "~/Dropbox/1Projects/programming/derivations-site/testpost.org"))
    ;; (setq file-path (expand-file-name "/home/chris/Dropbox/org/notes/software/site.org"))
    )

  (let* (relative-dir-path)
    (unless base-dir
      (setq base-dir (file-name-as-directory (concat (get-next-git-root) "org"))))
    (setq relative-dir-path (file-relative-name file-path base-dir))
    (with-temp-buffer
      (insert-file-contents file-path)
      (org-mode)
      (org-element-parse-buffer)
      (let* ((title (org-element-property :value (car (org-global-props-get-plist "TITLE"))))
             (date (org-element-property :value (car (org-global-props-get-plist "DATE"))))
             (post-type (org-element-property :value (car (org-global-props-get-plist "POST"))))
             (last-modified-date (format-time-string "%Y-%m-%d"
                                                     (file-attribute-status-change-time (file-attributes file-path)))))
        `(,title
          ,date
          ,last-modified-date
          ,(file-name-base file-path)
          ,(file-name-directory file-path)
          ,post-type)
        (make-post-metadata :title title
                            :date date
                            :last-modified-date last-modified-date
                            :file-name-base (file-name-base file-path)
                            :relative-dir-path (file-name-directory file-path)
                            :post-type post-type)))))

(defun print-post-metadata-into-org (pm-instance project-root-dir)
  ""
  (let* ((source-filepath (concat (file-name-as-directory (post-metadata-relative-dir-path pm-instance))
                            (post-metadata-file-name-base pm-instance)
                            ".org"))
         (date-str (post-metadata-date pm-instance)))
    (insert
     (concat "<div class=\"post-container-div\">\n"
             "<span class=\"posted\">\n"
             (when date-str date-str)
             "</span>\n"
             "<span class=\"post-link\">\n"
             "<a href=\""
             (file-relative-name (get-publish-target-filepath project-root-dir
                                                              source-filepath)
                                 (get-projects-publish-dir-from-root-dir project-root-dir))
             "\">"
             (post-metadata-title pm-instance)
             "</a>\n"
             "</span>\n"
             "</div>\n"))))


;; ------ formatting, i.e. getting the html for different elements ------

(defun cs-html-format-title-html (&optional title)
  ""
  (concat (when title (concat "<h2 " "style=\"text-align:center;\"" ">" title "</h2>"))))

(defun cs-html-format-slidingtopbar-html (&optional youtube-url github-url about-page-link legal-page-link)
  ""
  (concat
   "\n
<div class=\"container\">\n"
   (when github-url (concat "<a class=\"projectlink\" href=" (prin1-to-string github-url) ">Github</a>\n"))
   (when youtube-url (concat "<a class=\"projectlink\" href=" (prin1-to-string youtube-url) ">YouTube</a>\n"))
   (when about-page-link (concat "<a class=\"aboutlink\" href=" (prin1-to-string about-page-link) ">About</a>\n"))
   (when about-page-link (concat "<a class=\"aboutlink\" href=" (prin1-to-string legal-page-link) ">General & Legal Info</a>\n"))
   "</div>"))

(defun cs-html-format-project-description-block (title-str rel-link-to-html &optional project-name)
  ""
  (unless project-name
    (setq project-name (file-name-nondirectory (directory-file-name (file-name-directory rel-link-to-html)))))
  (unless (or (not title-str) (not (string-equal title-str "")))
    (setq title-str project-name))
  (concat "<div class=\"project-container-div\">\n"
          "<p>\n"
          "<a href=" (prin1-to-string rel-link-to-html) ">\n"
          title-str
          "</a>\n"
          "</p>\n"
          "<p class=\"posted\">"
          "GitHub: <a style=\"text-decoration: none;\" href=" cs-my-github-page-url
          project-name ">\n"
          project-name
          "</a>\n"
          "</p>"
          "</div>"))

;; ------- cleaning out the produced html of a projcet --------
(defun cs-clean-project-publish-buffer (&optional root-dir prompt subdir-rel-path)
  (interactive)
  (unless root-dir
    (setq root-dir (get-next-git-root))
    (unless root-dir
      (user-error "Root git dir not found")))
  (with-output-to-temp-buffer publish-buffer-name
    (shell-command
     (read-shell-command (if prompt prompt "Run the cleaning command like this: ")
                         (concat "cd "
                                 (prin1-to-string root-dir)
                                 " ; "
                                 ;; " && rm -rvf *.elc 2>/dev/null "
                                 " find "
                                 (prin1-to-string (concat (file-name-as-directory www-dir-name)
                                                          subdir-rel-path))
                                 " -type f,d -exec rm -rf {} + "
                                 " ; "
                                 " rm -rvf ~/.org-timestamps/* ; "))
                   publish-buffer-name)))





(provide 'cs-org-publish-utils)
;;; cs-org-publish-utils.el ends here
