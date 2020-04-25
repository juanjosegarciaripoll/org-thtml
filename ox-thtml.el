;;; ox-thtml.el --- Handlebar-style templates for org-mode

;; Copyright (C) 2019 Juan Jose Garcia Ripoll

;; Author: Juan JosÃ© GarcÃ­a Ripoll <juanjose.garciaripoll@gmail.com>
;; URL: http://juanjose.garciaripoll.com

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; We ensure the org infrastructure
(require 'org)
;; We need this for LOOP
(require 'cl)

(defvar templated-html-site-title "Homepage"
  "Default title for every page in the exported site")

(defvar templated-html-site-description "Personal homepage exported with org-thtml"
  "Default description for every page in the exported site")

(defvar templated-html-site-url "http://nowhere.org"
  "Full URL for the root of the site exported by org-thtml")

(require 'ox-publish)

(org-export-define-derived-backend 'templated-html 'html
  :translate-alist '((template . templated-html-template-fun)))

(defun templated-html-template-fun (contents info)
  (let ((template (plist-get info :html-template)))
    (if template
        (funcall template contents info)
      (org-html-template contents info))))

(defun org-html-publish-to-templated-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'templated-html filename
              (concat "." (or (plist-get plist :html-extension)
                      org-html-extension
                      "html"))
              plist pub-dir))

(defvar templated-html--current-template nil)

(defun templated-html-load-template (filename)
  (templated-html--byte-compile (templated-html--load-template filename)))

(defun templated-html--load-template (filename)
  (let ((templated-html--current-template filename))
    (with-temp-buffer
      (insert-file-contents filename)
      (templated-html--read-block))))

(defun templated-html--image-name (info)
  (let (image)
    (org-element-map (plist-get info :parse-tree) 'keyword
      (lambda (k)
        (when (equal (org-element-property :key k) "IMAGE")
          (setq image (org-element-property :value k)))))
    image))

(defun templated-html--collect-images (info)
  (let (images)
    (org-element-map (plist-get info :parse-tree) 'link
      (lambda (k)
        (when (equal (org-element-property :type k) "file")
          (let ((path (org-element-property :path k)))
            (when (string-match ".\\(jpe?g\\|png\\)" path)
              (setq images (cons path images)))))))
    (nreverse images)))

(defun templated-html--byte-compile (forms)
  (byte-compile
       `(lambda (contents info)
          (let* ((real-root (expand-file-name (or (plist-get info :root-directory)
                                                  (plist-get info :base-directory))))
                 (input-file (plist-get info :input-file))
                 (input-url (templated-html--absolute-path input-file real-root "html"))
                 (root (templated-html--relative-path input-file real-root))
                 (date (org-publish-find-date input-file info))
                 (description (if (plist-get info :description)
                                  (org-export-data (plist-get info :description) info)
                                templated-html-site-description))
                 (site-url (or (plist-get info :site-url) templated-html-site-url))
                 (with-title (plist-get info :with-title))
                 (title (if (plist-get info :title)
                            (org-export-data (plist-get info :title) info)
                          templated-html-site-title))
                 (image (or (templated-html--image-name info)
                            (car (templated-html--collect-images info))))
                 image-url image-width image-height)
             (when image
               (setq image (expand-file-name image))
               (let* ((image-object (create-image image)))
                 (when image-object
                   (let ((image-size (image-size image-object)))
                     (message "Image size %S for %s" image-size image)
                     (setq image-width (format "%d" (round (* (car image-size) (frame-char-width))))
                           image-height (format "%d" (round (* (cdr image-size) (frame-char-height)))))))
                 (setq image-url (templated-html--absolute-path image real-root)))
               (message "Image: %s" image)
               (message "Root: %s" real-root)
               (message "Image url: %s" image-url)
               )
             ,forms))))

(defvar templated-html-helper-alist
  '((:include . templated-html--include)
    (:if . templated-html--if)
    (:each . templated-html--each)
    (:endeach . templated-html--syntax-error)
    (:endif . templated-html--syntax-error)))

(defun templated-html--read-block (&rest end-marks)
  "Read the template buffer, transforming it into lisp
statements. It reads the HTML until a handleblar expression
{{form}} is found. Text in between expressions is inserted as is
into the template. Forms {{a b c ...}} are interpreted as lisp
expressions (a b c ...) except when they consist of just one
element {{a}} which is read as-is. Special forms where 'a' is one
of :if, :each, :include are delegated to helper functions."
  (loop with forms = nil
        with head = nil
        for item = (print (templated-html--read-form))
        while (not (or (eq item ':eof)
                       (member item end-marks)))
        do (cond ((null item))
                 ((atom item)
                  ;; Maybe transcode characters to HTML entities?
                  (push item forms))
                 ((setq head (assoc (car item) templated-html-helper-alist))
                  (push (funcall (cdr head) (rest item)) forms))
                 (t
                  (push item forms)))
        finally return `(concat ,@(nreverse forms))))

(defun templated-html--read-form ()
  "Extract the next form in the template. It can be either a
string, or an s-expression enclosed in a handlerbar {{form}}."
  (let ((beg (point)))
    (cond ((= beg (point-max))
           ':eof)
          ((and (> beg (point-min))
                (eq (char-before) ?{))
           ;; We are reading an s-exp from a handlebar template
           (if (re-search-forward "\\([^}]+\\)}}" nil 'noerror)
               (let* ((data (match-data t))
                      (form (buffer-substring (nth 2 data) (nth 3 data)))
                      (s-exp (car (read-from-string (format "(%s)" form)))))
                 (if (= (length s-exp) 1)
                     (car s-exp)
                   s-exp))
             (error "Invalid template file %s" filename)))
          (t
           ;; We are reading a string until the next handlebar expression
           (if (search-forward "{{" nil 'noerror)
               (buffer-substring beg (- (point) 2))
             (buffer-substring beg (point-max)))))))

(defun templated-html--relative-path (input base)
  (apply 'concatenate 'string
           (loop for i in (rest (split-string (file-relative-name input-file base)
                                        "[/\\]"))
                 collect "../")))

(defun templated-html--absolute-path (input-file real-root &optional ext)
  (let ((file-name (concat "/" (file-relative-name input-file real-root))))
    (if ext
        (concat (file-name-sans-extension file-name) "." ext)
      file-name)))

(defun templated-html--include (form)
  "Handler for {{:include filename}} statements."
  (let* ((value (car form))
         (filename (expand-file-name (if (stringp value) value (format "%s" value))
                                     (file-name-directory templated-html--current-template))))
    (templated-html--load-template filename)))

(defun templated-html--if (form)
  "Handler for {{:if condition}}...{{:endif}} blocks."
  `(if ,(car form)
       ,(templated-html--read-block :endif)
     ""))

(defun templated-html--each (form)
  "Handler for {{:each list}}...{{:endeach}} blocks."
  `(apply 'concat
          (loop for item in ,(car form)
                collect ,(templated-html--read-block :endeach))))

(defun org-simple-rss--body (title description root list)
  "Generate RSS feed, as a string.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (with-temp-buffer
    (insert (format "<?xml version='1.0' encoding='UTF-8' ?>
<rss version='2.0'>
<channel>
 <title>%s</title>
 <link>%s</link>
 <description>%s</description>"
                    title root description))
    (dolist (l list)
      (when (and (listp l) (stringp (car l)))
        (insert (car l))))
    (insert "</channel>\n</rss>")
    (buffer-string)))

(defun org-simple-rss--entry (entry style project)
  "Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
  (message "org-publish-entry %s" entry)
  (cond ((not (directory-name-p entry))
         (let* ((rss-root (or (org-publish-property :rss-root project) ""))
                (file (org-publish--expand-file-name entry project))
                (title (org-publish-find-title entry project))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat rss-root (file-name-sans-extension entry) ".html")))
           (format "  <item>\n   <title>%s</title>\n   <link>%s</link>\n   <pubDate>%s</pubDate>\n  </item>\n" title link date)))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun org-simple-rss-alist (name &rest alist)
  (let* ((project (cons name alist))
         (rss-title (org-publish-property :rss-title project))
         (rss-filename (org-publish-property :rss-filename project))
         (rss-description (org-publish-property :rss-description project))
         (rss-root (org-publish-property :rss-root project)))
    (unless rss-title
      (user-error "Missing :rss-title in org-publish project %s" name))
    (unless rss-filename
      (user-error "Missing :rss-filename in org-publish project %s" name))
    (unless rss-description
      (user-error "Missing :rss-description in org-publish project %s" name))
    (unless rss-root
      (user-error "Missing :rss-root in org-publish project %s" name))
    (append
     (list name
           :publish-function (lambda (a b c) nil)
           :auto-sitemap t
           :sitemap-title rss-title
           :sitemap-style 'list
           :sitemap-filename rss-filename
           :sitemap-sort-files 'anti-chronologically
           :publishing-function (byte-compile '(lambda (&rest r) nil))
           :sitemap-format-entry 'org-simple-rss--entry
           :sitemap-function
           (byte-compile `(lambda (title list)
                            (org-simple-rss--body title ,rss-description ,rss-root list))))
     alist)))

(defun templated-html-create-sitemap-xml (output directory base-url &rest regexp)
  (let* ((rx (or regexp "\\.html")))
    (with-temp-file output
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset
      xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"
            http://www.sitemaps.org/schemas/sitemap/0.9
            http://www.sitemaps.org/schemas/sitemap/09/sitemap.xsd\">\n")
      (loop for file in (directory-files-recursively directory rx)
            do (insert (format "<url>\n <loc>%s/%s</loc>\n <priority>0.5</priority>\n</url>\n"
                               base-url (file-relative-name file directory))))
      (insert "</urlset>"))))
