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

(defvar-local magit-gerrit-account-self nil
  "Account alist representing currently authenticated user.")

(defun magit-gerrit--patchset-url (changeset revision)
  (concat magit-gerrit-url
          "/a/changes/" changeset
          "/revisions/" revision))

(defun magit-gerrit--get (url)
  "Make a GET request to the URL."
  (let ((url-request-method "GET"))
    (magit-gerrit--request url)))

(defun magit-gerrit--post (url data method)
  "Make a METHOD request to the URL with the given DATA.

DATA should be an alist
Supported METHODs are 'PUT' and 'POST'."
  (let ((url-request-method method)
        (url-request-extra-headers
         '(("Content-Type" . "application/json; charset=UTF-8")))
        (url-request-data (json-encode-alist data)))
    (magit-gerrit--request url)))

(defun magit-gerrit--request (url)
  "Fetch json from the URL into the temporary buffer and return the parsed result."
  (with-temp-buffer
    (url-insert-file-contents url)
    ;; gerrit API returns 5 special symbols before any response
    ;; to prevent Cross Site Script Inclusion (XSSI) attacks
    ;; we skip these symbols to parse correctly
    (let* ((special-symbols-offset 5)
           (json-string
            (buffer-substring-no-properties
             special-symbols-offset (point-max))))
      (json-read-from-string json-string))))

(defun magit-gerrit--get-account ()
  "Return currently authorized user's account alist object.
Try to get value from the global variable `magit-gerrit-account-self'.
Otherwise, fetch account info through the API."
  (if magit-gerrit-account-self
      magit-gerrit-account-self
    (let* ((account-url (concat magit-gerrit-url "/a/accounts/self"))
           (account (magit-gerrit--get account-url)))
      (setq magit-gerrit-account-self account)
      account)))

(defun magit-gerrit--extract-author (comment)
  "Return COMMENT's author.
If author property of the COMMENT is nil, return currently authorized user."
  (alist-get 'name
             (if-let ((comment-author (alist-get 'author comment)))
                 comment-author
               (magit-gerrit--get-account))))

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
       (oset comment-info author (magit-gerrit--extract-author comment))
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

(defun magit-gerrit-post-draft (draft)
  "Send DRAFT to the gerrit throught the API."
  (-let* (((&alist 'changeset changeset 'revision revision)
           magit-gerrit-review-alist)
          (url (concat (magit-gerrit--patchset-url changeset revision)
                       "/drafts"))
          (data `(
                  ("path" . ,(oref draft file))
                  ("range" . ,(oref draft range))
                  ("message" . ,(oref draft message))
                  ("side" . ,(if (get-text-property 0 'base revision)
                                 "PARENT"
                               "REVISION")))))
    (magit-gerrit--post url data "PUT")))

;;; _
(provide 'magit-gerrit-requests)
;;; magit-gerrit-requests.el ends here
