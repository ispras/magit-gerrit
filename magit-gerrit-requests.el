;;; magit-gerrit-requests.el --- interact with Gerrit API -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Brian Fransioli

;; Author: Brian Fransioli <assem@terranpro.org>
;; Maintainer:
;;    Valeriy Savchenko     <vsavchenko@ispras.ru>
;;    Konstantin Sorokin    <ksorokin@ispras.ru>
;;    Georgiy Pankratenko   <gpankratenko@ipsras.ru>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'eieio)
(require 'json)

(require 'magit-gerrit-comment)

(defun magit-gerrit--get (url)
  "Fetch json from the URL into the temporary buffer and return the parsed result."
  (with-temp-buffer
    (url-insert-file-contents url)
    (let* ((special-symbols-offset 5)
           (json-string
            (buffer-substring-no-properties
             special-symbols-offset (point-max))))
      (json-read-from-string json-string))))

(defun magit-gerrit--extract-comment-range (comment)
  "Return COMMENT range based on what fields this COMMENT has."
  ;; when COMMENT has range attribute return it
  ;; when COMMENT has line attribute return zero length range within this line
  ;; otherwise, return zero length range withing the 0-th line
  (if-let ((comment-range (alist-get 'range comment)))
        comment-range
      (if-let ((comment-line (alist-get 'line comment)))
            `((start_line . ,comment-line) (start_character . 0)
              (end_line . ,comment-line) (end_character . 0))
          `((start_line . 0) (start_character . 0)
              (end_line . 0) (end_character . 0)))))

(defun magit-gerrit--parse-file-comments (comments &optional drafts)
  "Given COMMENTS and DRAFTS lists return corresponding list of commentinfo objects."
  (seq-map
   (lambda (comment)
     (let ((comment-info (magit-gerrit--commentinfo)))
       (oset comment-info author (alist-get 'name (alist-get 'author comment)))
       (oset comment-info date
             (apply 'encode-time
                    (parse-time-string (alist-get 'updated comment))))
       (oset comment-info draft drafts)
       (oset comment-info message (alist-get 'message comment))
       (oset comment-info range (magit-gerrit--extract-comment-range comment))
       (oset comment-info side (alist-get 'side comment))
       comment-info))
   comments))

(defun magit-gerrit--parse-comments (response &optional drafts)
  "Parse per file comments from the RESPONSE.

If DRAFTS is not nil treat response as draft comments."
  (seq-map
   (-lambda ((path . comments))
     (cons (symbol-name path)
           (magit-gerrit--parse-file-comments comments drafts)))
   response))

;;; _
(provide 'magit-gerrit-requests)
;;; magit-gerrit-requests.el ends here
