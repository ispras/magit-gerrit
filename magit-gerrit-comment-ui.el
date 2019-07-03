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
(require 'magit-gerrit-requests)

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

(defun magit-gerrit--add-comments (revision comments &optional buffer)
  "Add COMMENTS's overlays for the REVISION in the BUFFER.

Break up comments for the BASE and comments for the patchset
according to the REVISION.
If BUFFER is nil show comments in the current buffer."
  (let ((filtered-comments
         (if (get-text-property 0 'base revision)
             (seq-filter (lambda (comment)
                           (oref comment side))
                         comments)
           (seq-filter (lambda (comment)
                         (not (oref comment side)))
                       comments))))
    (magit-gerrit-create-overlays filtered-comments buffer)))

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
                  (format "%s %s %s"
                          (format-time-string magit-gerrit-comment-ts-format
                                              (oref comment-info date))
                          (oref comment-info author)
                          (if (oref comment-info draft) "[Draft]" ""))
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
  (let ((comment-ov (overlay-get ov 'comment-overlay)))
    (overlay-put comment-ov 'after-string
                 (magit-gerrit-create-comment-text-string
                  (overlay-get comment-ov 'comment-info) active))
    (overlay-put ov 'face
                 (if active 'magit-gerrit-active-range-face
                   'magit-gerrit-range-face))))

(defconst magit-gerrit-maxpriority 1000
  "Maximum prioity value for magit-gerrit comment overlays.")

(defun magit-gerrit-new-comment-overlay-priority (pos)
  "Compute priority for the new comment overlay at the given position POS.

The new priority is based on whether there already exist some comment overlays.
If there are no overlays return 'magit-gerrit-maxpriority' else, returns
the lowest priority among the existing overlays minus 1.

FIXME: currently we do not check for cases when the new prioity drops below 0"
  (let* ((existing-overlays (magit-gerrit-comment-overlays-in pos))
         (min-priority-ov (last existing-overlays)))
    (if min-priority-ov
        (1- (overlay-get (car min-priority-ov) 'priority))
      magit-gerrit-maxpriority)))

(defun magit-gerrit-comment-overlays-in (start &optional end)
  "Return a list of magit-gerrit-comment overlays in the given region.

START and END are buffer positions.

If END is nil return only comment overlays corresponding to START position"
  (magit-gerrit-overlays-in start end 'magit-gerrit-comment-ov))

(defun magit-gerrit-range-overlays-in (start &optional end)
  "Return a list of magit-gerrit-range overlays in the given region.

START and END are buffer positions.

If END is nil return only comment overlays corresponding to START position"
  (magit-gerrit-overlays-in start end 'magit-gerrit-range-ov))

(defun magit-gerrit-overlays-in (start end type)
  "Get the sorted list of overlays of type TYPE.

The sorting is multilevel.  The first one is sorting in ascending
order of start position.  The second one is sorting in decreasing order of
overlay priority.

START end END are buffer positions."
  (let ((overlays (seq-filter (lambda (ov) (overlay-get ov type))
                              (overlays-in start (if end end start)))))
    (seq-sort (lambda (a b)
                (cond ((< (overlay-start a) (overlay-start b)) t)
                      ((= (overlay-start a) (overlay-start b))
                       (> (overlay-get a 'priority) (overlay-get b 'priority)))
                      (t nil)))
              overlays)))

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
    (overlay-put range-ov 'priority comment-text-ov-priority)

    (overlay-put comment-text-ov
                 'after-string
                 (magit-gerrit-create-comment-text-string comment-info))
    (overlay-put comment-text-ov 'priority comment-text-ov-priority)
    (overlay-put comment-text-ov 'magit-gerrit-comment-ov t)
    (overlay-put comment-text-ov 'comment-info comment-info)

    ;; Add comment text overlay as a child property
    (overlay-put range-ov 'comment-overlay comment-text-ov)
    ;; Add comment-info as a property to the corresponding comment overlay
    ;; for possible references.
    range-ov))

;; FIXME: find a better way to do this
(defvar-local magit-gerrit--active-comment-ov nil
  "Stores currently active comment in buffer")

(defun magit-gerrit-point-in-ov-p (ov)
  "Check whether current point is in the region of the given overlay OV."
  (and (<= (point) (overlay-end ov)) (>= (point) (overlay-start ov))))

(defun magit-gerrit--nth-next-overlay(ov overlays n)
  "Get N-th next overlay in OVERLAYS after the given OV.

If there is no such overlay return nil."
  (when-let* ((pos (position ov overlays))
              (nth-pos (+ pos n)))
    (when (>= nth-pos 0) (nth nth-pos overlays))))

(defun magit-gerrit--next-overlay ()
  "Get the next available overlay.

If there are no next overlays return nil."
  (if magit-gerrit--active-comment-ov
      (let* ((start (overlay-start magit-gerrit--active-comment-ov))
             (overlays (magit-gerrit-range-overlays-in start (point-max))))
        (magit-gerrit--nth-next-overlay magit-gerrit--active-comment-ov
                                        overlays 1))
    (nth 0 (magit-gerrit-range-overlays-in (point) (point-max)))))

(defun magit-gerrit--prev-overlay ()
  "Get the previous available overlay.

If there are no previous overlays return nil."
  (if magit-gerrit--active-comment-ov
      (let* ((start (overlay-start magit-gerrit--active-comment-ov))
             (overlays (magit-gerrit-range-overlays-in (point-min) (1+ start))))
        (magit-gerrit--nth-next-overlay magit-gerrit--active-comment-ov
                                        overlays -1))
    (car (last (magit-gerrit-range-overlays-in  (point-min) (1+ (point)))))))

(defun magit-gerrit-next-comment ()
  "Go to the gerrit comment next to the current position."
  (interactive)
  (if-let ((next-overlay (magit-gerrit--next-overlay)))
      (magit-gerrit-goto-comment next-overlay)
    (message "No more commments")))

(defun magit-gerrit-prev-comment ()
  "Go to the gerrit comment previous to the current position."
  (interactive)
  (if-let ((prev-overlay (magit-gerrit--prev-overlay)))
      (magit-gerrit-goto-comment prev-overlay)
    (message "No previous comments")))

(defun magit-gerrit-goto-comment (range-ov)
  "Go to the comment specified by the RANGE-OV overlay."
  ;; Set current comment as inactive
  (when magit-gerrit--active-comment-ov
    (magit-gerrit-toggle-comment-overlay-active magit-gerrit--active-comment-ov nil))

  ;; Update current active comment and go there
  (setq magit-gerrit--active-comment-ov range-ov)
  (magit-gerrit-toggle-comment-overlay-active magit-gerrit--active-comment-ov t)
  (goto-char (overlay-start magit-gerrit--active-comment-ov)))

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

(defun magit-gerrit--char-at-pos (&optional pos)
  "Given position POS in current buffer get its column number."
  (save-excursion (goto-char (if pos pos (point))) (current-column)))

(defun magit-gerrit--pos-to-line-col (start end)
  "Translate given position range to line-column range.

START and END are buffer positions."
  (list (cons 'start_line (line-number-at-pos start))
        (cons 'start_character (magit-gerrit--char-at-pos start))
        (cons 'end_line (line-number-at-pos end))
        (cons 'end_character (magit-gerrit--char-at-pos end))))

(defun magit-gerrit--new-comment-range ()
  "Compute range for the new comment.

Resulting range depends on several conditions.  If the range was
selected manually it will be the resulting range.

Othewrise pick the (point, point) region or currently active comment's region
if the point is inside the latter."
  (cond
   ;; Handle selected region case
   ((use-region-p)
    (magit-gerrit--pos-to-line-col (region-beginning) (region-end)))

   ;; Handle case when we are inside the active comment's region
   ((and magit-gerrit--active-comment-ov
         (magit-gerrit-point-in-ov-p magit-gerrit--active-comment-ov))
    (magit-gerrit--pos-to-line-col
     (overlay-start magit-gerrit--active-comment-ov)
     (overlay-end magit-gerrit--active-comment-ov)))

   ;; Handle default case
   (t (magit-gerrit--pos-to-line-col (point) (point)))))

(defun magit-gerrit-add-comment ()
  "Add new draft comment for the current region or point."
  (interactive)
  (magit-gerrit-create-comment-overlays
   (magit-gerrit--commentinfo
    :author (alist-get 'name (magit-gerrit--get-account))
    :date (current-time)
    :text (read-from-minibuffer "Comment message: ")
    :file (magit-current-file)
    :range (magit-gerrit--new-comment-range)
    :draft t)))
;;; _
(provide 'magit-gerrit-comment-ui)
;;; magit-gerrit-comment-ui.el ends here
