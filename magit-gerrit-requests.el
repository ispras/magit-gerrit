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

(defclass magit-gerrit--commentinfo ()
  ((author    :initform nil)
   (date      :initform nil)
   (line      :initform nil)
   (message   :initform nil)
   (range     :initform nil))
  "Data class for a changeset/patchset comment.")

(defun magit-gerrit--get (url)
  "Fetch json from the URL into the temporary buffer and return the parsed result."
  (with-temp-buffer
    (url-insert-file-contents url)
    (let* ((special-symbols-offset 5)
           (json-string
            (buffer-substring-no-properties
             special-symbols-offset (point-max))))
      (json-read-from-string json-string))))

(defun magit-gerrit--parse-file-comments (comments)
  "Given COMMENTS list return corresponding list of commentinfo objects sorted by date."
  (seq-sort
   (lambda (prev-comment next-comment)
     (time-less-p
      (oref prev-comment date)
      (oref next-comment date)))
   (seq-map
    (lambda (comment)
      (let ((comment-info (magit-gerrit--commentinfo)))
        (oset comment-info author (alist-get 'name (alist-get 'author comment)))
        (oset comment-info date
              (apply 'encode-time
                     (parse-time-string (alist-get 'updated comment))))
        (oset comment-info message (alist-get 'message comment))
        (let ((comment-range (alist-get 'range comment)))
          (if comment-range
              (oset comment-info range comment-range)
            (let ((comment-line (alist-get 'line comment)))
              (oset comment-info range
                    `((start_line . ,comment-line) (start_character . 1)
                      (end_line . ,comment-line) (end_character . 1))))))
        comment-info))
   comments)))

(defun magit-gerrit--parse-comments (response)
  "Parse per file comments from the RESPONSE."
  (seq-map
   (-lambda ((path . comments))
    (cons (symbol-name path)
          (magit-gerrit--parse-file-comments comments)))
   response))

;;; _
(provide 'magit-gerrit-requests)
;;; magit-gerrit-requests.el ends here
