;;; magit-gerrit-comments.el --- magit-gerrit comment functionality -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2013-2019 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;; Maintainer:
;;    Valeriy Savchenko     <vsavchenko@ispras.ru>
;;    Konstantin Sorokin    <ksorokin@ispras.ru>
;;    Georgiy Pankratenko   <gpankratenko@ipsras.ru>

;;; Code:

(require 'eieio)

;; TODO: move this somewhere else
(defgroup magit-gerrit-group nil
  "Group for magit-gerrit related settings")

(defface magit-gerrit-comment-face
  '((t :foreground "gray50"))
  "Face used for displaying gerrit comment text"
  :group 'magit-gerrit-group)

(defface magit-gerrit-comment-heading-face
  '((t :inherit magit-gerrit-comment-face :weight bold :box t))
  "Face used for displaying comment heading"
  :group 'magit-gerrit-group)

(defface magit-gerrit-comment-range-face
  '((t :background "gray50"))
  "Face for highlighting comment region"
  :group 'magit-gerrit-group)

(defface magit-gerrit-active-comment-face
  '((t :inherit magit-gerrit-comment-face
       :foreground "gray80"))
  "Face for highlighting currently selected comment"
  :group 'magit-gerrit-group)

(defface magit-gerrit-active-range-face
  '((t :inherit magit-gerrit-comment-range-face
       :background "gray80"))
  "Face for highlighting currently selected comment's range"
  :group 'magit-gerrit-group)

(defclass magit-gerrit--commentinfo ()
  ((author    :initform nil :initarg :author)
   (date      :initform nil :initarg :date)
   (message   :initform nil :initarg :text)
   (file      :initform nil :initarg :file)
   (range     :initform nil :initarg :range))

  "Class that binds together all information related to the single comment.

AUTHOR  is a string representing commment author
DATE    is a string representing comment post date
MESSAGE is a string representing actual comment message
FILE    is a string representing filename this comment refers to
RANGE   is an alist with 'start_line', 'start_col', 'end_line', 'end_col' keys")

(defun pos-at-line-col (line col)
  "Translate line and column to the position in the given buffer.

LINE is the buffer line number
COL  is the buffer column number"
  (save-excursion
    (goto-char (point-min))
    ;; TODO: check corner cases
    (forward-line (1- line))
    (move-to-column (1- col))
    (point)))

(defun magit-gerrit-create-comment-text-string (comment-info)
  "Create comment text string with face properties.

COMMENT-INFO is an instance of magit-gerrit-commentinfo"
  (let ((heading (propertize (format "%s %s "
                                     (oref comment-info date)
                                     (oref comment-info author))
                             'face 'magit-gerrit-comment-heading-face))
        (content (propertize (oref comment-info message)
                             'face 'magit-gerrit-comment-face)))
    (format "\n%s\n\n%s\n" heading content)))

(defun magit-gerrit-create-comment-overlays (comment-info)
  "Create overlays for the provided comment info.

COMMENT-INFO is an instance of magit-gerrit--commentinfo"
  (let* ((range (oref comment-info range))
         (start-pos (pos-at-line-col (alist-get 'start_line range)
                                     (alist-get 'start_col range)))
         (end-line (alist-get 'end_line range))
         (end-pos (pos-at-line-col end-line (alist-get 'end_col range)))

         ;; Create overlay in the comment range to highlight it
         (range-ov (make-overlay start-pos end-pos))

         ;; Create empty overlay starting at the next line for the comment
         ;; text. We are creating separate overlay here, because the range
         ;; overlay does not necessarily end at the line end, which means
         ;; that 'after-string' will uglify the buffer contents.
         (text-ov-pos (pos-at-line-col (1+ end-line) 1))
         (comment-text-ov (make-overlay text-ov-pos text-ov-pos)))

    (overlay-put range-ov 'face 'magit-gerrit-comment-range-face)
    (overlay-put comment-text-ov
                 'after-string
                 (magit-gerrit-create-comment-text-string comment-info))

    ;; Add range-overlay as a child property
    (overlay-put comment-text-ov 'range-overlay range-ov)))

;;; _
(provide 'magit-gerrit-comments)
;;; magit-gerrit-comments.el ends here
