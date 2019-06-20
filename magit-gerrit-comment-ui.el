;;; magit-gerrit-comment-ui.el --- magit-gerrit comment ui -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2013-2019 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;; Maintainer:
;;    Valeriy Savchenko     <vsavchenko@ispras.ru>
;;    Konstantin Sorokin    <ksorokin@ispras.ru>
;;    Georgiy Pankratenko   <gpankratenko@ipsras.ru>

;;; Code:

(require 'magit-gerrit-comment)

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

(defface magit-gerrit-active-comment-heading-face
  '((t :inherit magit-gerrit-comment-heading-face
       :foreground "gray80"))
  "Face for highlighting currently selected comment's heading"
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

(defcustom magit-gerrit-comment-ts-format
  "%b %d %H:%M %Y"
  "The timestamp format used in gerrit comment overlays."
  :group 'magit-gerrit-group
  :type 'string)

(defun magit-gerrit-pos-at-line-col (line col &optional buffer)
  "Translate line and column to the position in the given buffer.

LINE is the buffer line number
COL  is the buffer column number
BUFFER if not nil perform translation in the given buffer, otherwise
do this in the current one."
  (let ((target-buffer (if buffer buffer (current-buffer))))
    (with-current-buffer target-buffer
      (save-excursion
        (goto-char (point-min))
        ;; We need to go forward `line'-1 line because line numeration begins at
        ;; 1. Special case when `line' is 0 is handled correctly because for
        ;; negative arguments `forward-line' moves backwards.
        (forward-line (1- line))
        ;; For columns we move exactly `col' times because numeration starts
        ;; from 0
        (move-to-column col)
        (point)))))

(defun magit-gerrit-create-comment-text-string (comment-info &optional active)
  "Create comment text string with face properties.

COMMENT-INFO is an instance of magit-gerrit-commentinfo
ACTIVE whether to highlight text as active"
  (let ((heading (propertize
                  (format "%s %s "
                          (format-time-string magit-gerrit-comment-ts-format
                                              (oref comment-info date))
                          (oref comment-info author))
                  'face (if active
                            'magit-gerrit-active-comment-heading-face
                          'magit-gerrit-comment-heading-face)))
        (content (propertize (oref comment-info message)
                             'face (if active
                                       'magit-gerrit-active-comment-face
                                     'magit-gerrit-comment-face))))
    (format "\n%s\n\n%s\n" heading content)))

(defun magit-gerrit-toggle-comment-overlay-active (ov active)
  "Toggle the active state of the given comment overlay OV.

ACTIVE if non-nil make the given comment overlay active"
  (overlay-put ov 'after-string
               (magit-gerrit-create-comment-text-string
                (overlay-get ov 'comment-info) active)))

(defconst magit-gerrit-maxpriority 1000
  "Maximum prioity value for magit-gerrit comment overlays.")

(defun magit-gerrit-new-comment-overlay-priority (pos)
  "Compute priority for the new comment overlay at the given position POS.

The new priority is based on whether there already exist some comment overlays.
If there are no overlays return 'magit-gerrit-maxpriority' else, returns
the lowest priority among the existing overlays minus 1.

FIXME: currently we do not check for cases when the new prioity drops below 0"
  (let* ((existing-overlays (magit-gerrit-comment-overlays-at pos))
         (min-priority-ov (last existing-overlays)))
    (if min-priority-ov
        (1- (overlay-get (car min-priority-ov) 'priority))
      magit-gerrit-maxpriority)))

(defun magit-gerrit-comment-overlay-p (ov)
  "Check whether given overlay OV corresponds to the gerrit comment."
  (overlay-get ov 'magit-gerrit-comment-ov))

(defun magit-gerrit-comment-overlays-at (pos)
  "Return a list of magit-gerrit-comment overlays at the given position POS."
  (seq-sort (lambda (a b) (< (overlay-get a 'priority)
                             (overlay-get b 'priority)))
            (seq-filter 'magit-gerrit-comment-overlay-p
                        (overlays-in pos pos))))

(defun magit-gerrit-create-comment-overlays (comment-info &optional buffer)
  "Create overlays for the provided comment info.

COMMENT-INFO is an instance of magit-gerrit--commentinfo
BUFFER if non nil create overlays in the given buffer, otherwise do
this in the current one"
  (let* ((range (oref comment-info range))
         (start-pos (magit-gerrit-pos-at-line-col
                     (alist-get 'start_line range)
                     (alist-get 'start_character range)
                     buffer))
         (end-line (alist-get 'end_line range))
         (end-pos (magit-gerrit-pos-at-line-col
                   end-line
                   (alist-get 'end_character range)
                   buffer))

         ;; Create overlay in the comment range to highlight it
         (range-ov (make-overlay start-pos end-pos buffer))

         ;; Create empty overlay starting at the next line for the comment
         ;; text. We are creating separate overlay here, because the range
         ;; overlay does not necessarily end at the line end, which means
         ;; that 'after-string' will uglify the buffer contents.
         (comment-text-ov-pos (magit-gerrit-pos-at-line-col
                               (1+ end-line) 0 buffer))
         ;; NOTE: it is necessary to compute priority BEFORE the new comment
         ;; overlay is actually created.
         (comment-text-ov-priority (magit-gerrit-new-comment-overlay-priority
                                    comment-text-ov-pos))
         (comment-text-ov (make-overlay comment-text-ov-pos
                                        comment-text-ov-pos
                                        buffer)))

    (overlay-put range-ov 'face 'magit-gerrit-comment-range-face)
    (overlay-put range-ov 'magit-gerrit-range-ov t)

    (overlay-put comment-text-ov
                 'after-string
                 (magit-gerrit-create-comment-text-string comment-info))
    (overlay-put comment-text-ov 'priority comment-text-ov-priority)
    (overlay-put comment-text-ov 'magit-gerrit-comment-ov t)

    ;; Add range-overlay as a child property
    (overlay-put comment-text-ov 'range-overlay range-ov)
    ;; Add comment-info as a property to the corresponding comment overlay
    ;; for possible references.
    (overlay-put comment-text-ov 'comment-info comment-info)
    comment-text-ov))

(defun magit-gerrit-create-overlays (comments &optional buffer)
  "Create overlays for each of the given comments.

COMMENTS is a list of magit-gerrit--commentinfo objects
BUFFER if non nil create overlays in the given buffer, otherwise do
this in the current one"
  (let ((sorted-comments
         (seq-sort (lambda (a b) (time-less-p (oref a date) (oref b date)))
                   comments)))
    (dolist (comment sorted-comments)
      (magit-gerrit-create-comment-overlays comment buffer))))

;;; _
(provide 'magit-gerrit-comment-ui)
;;; magit-gerrit-comment-ui.el ends here
