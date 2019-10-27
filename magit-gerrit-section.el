;;; magit-gerrit-section.el --- Functions for rendering gerrit section in Magit
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun magit-gerrit-string-trunc (str maxlen)
  (if (> (length str) maxlen)
      (concat (substring str 0 maxlen)
              "...")
    str))

(defun magit-gerrit-pretty-print-reviewer (name email crdone vrdone)
  (let* ((wid (1- (window-width)))
         (crstr (propertize
                 (if crdone (format "%+2d" (string-to-number crdone)) "  ")
                 'face '(magit-diff-lines-heading
                         bold)))
         (vrstr (propertize
                 (if vrdone (format "%+2d" (string-to-number vrdone)) "  ")
                 'face '(magit-diff-added-highlight
                         bold)))
         (namestr (propertize (or name "") 'face 'magit-refname))
         (emailstr (propertize (if email (concat "(" email ")") "")
                               'face 'change-log-name)))
    (format "%-12s%s %s" (concat crstr " " vrstr) namestr emailstr)))

(defun magit-gerrit-pretty-print-review (num subj owner-name &optional draft)
  ;; window-width - two prevents long line arrow from being shown
  (let* ((wid (- (window-width) 2))
         (numstr (propertize (format "%-10s" num) 'face 'magit-hash))
         (nlen (length numstr))
         (authmaxlen (/ wid 4))

         (author (propertize (magit-gerrit-string-trunc owner-name authmaxlen)
                             'face 'magit-log-author))

         (subjmaxlen (- wid (length author) nlen 6))

         (subjstr (propertize (magit-gerrit-string-trunc subj subjmaxlen)
                              'face
                              (if draft
                                  'magit-signature-bad
                                'magit-signature-good)))

         (authsubjpadding (make-string
                           (max 0 (- wid (+ nlen
                                            1
                                            (length author)
                                            (length subjstr))))
                           ? )))
    (format "%s%s%s%s\n"
            numstr subjstr authsubjpadding author)))

(defun magit-gerrit-wash-approval (approval)
  (let* ((approver (cdr-safe (assoc 'by approval)))
         (approvname (cdr-safe (assoc 'name approver)))
         (approvemail (cdr-safe (assoc 'email approver)))
         (type (cdr-safe (assoc 'type approval)))
         (verified (string= type "Verified"))
         (codereview (string= type "Code-Review"))
         (score (cdr-safe (assoc 'value approval))))

    (magit-insert-section (section approval)
      (insert (magit-gerrit-pretty-print-reviewer approvname approvemail
                                                  (and codereview score)
                                                  (and verified score))
              "\n"))))

(defun magit-gerrit-wash-approvals (approvals)
  (mapc #'magit-gerrit-wash-approval approvals))

(defun magit-gerrit-wash-review ()
  (let* ((beg (point))
         (jobj (json-read))
         (end (point))
         (num (cdr-safe (assoc 'number jobj)))
         (subj (cdr-safe (assoc 'subject jobj)))
         (owner (cdr-safe (assoc 'owner jobj)))
         (owner-name (cdr-safe (assoc 'name owner)))
         (owner-email (cdr-safe (assoc 'email owner)))
         (patchsets (cdr-safe (assoc 'currentPatchSet jobj)))
         ;; compare w/t since when false the value is => :json-false
         (isdraft (eq (cdr-safe (assoc 'isDraft patchsets)) t))
         (approvs (cdr-safe (if (listp patchsets)
                                (assoc 'approvals patchsets)
                              (assoc 'approvals (aref patchsets 0))))))
    (if (and beg end)
        (delete-region beg end))
    (when (and num subj owner-name)
      (magit-insert-section (section subj)
        (insert (propertize
                 (magit-gerrit-pretty-print-review num subj owner-name isdraft)
                 'magit-gerrit-jobj
                 jobj))
        (unless (oref (magit-current-section) hidden)
          (magit-gerrit-wash-approvals approvs))
        (add-text-properties beg (point) (list 'magit-gerrit-jobj jobj)))
      t)))

(defun magit-gerrit-wash-reviews (&rest args)
  (magit-wash-sequence #'magit-gerrit-wash-review))

(defun magit-insert-gerrit-reviews ()
  (let ((magit-git-executable "ssh")
        (magit-git-global-arguments nil)
        (command
         (split-string (gerrit-query (magit-gerrit-get-project)))))
    (magit-insert-section (gerrit-reviews)
      (magit-insert-heading "Reviews:")
      (magit-git-wash #'magit-gerrit-wash-reviews command)
      (insert "\n"))))

(provide 'magit-gerrit-section)

;;; magit-gerrit-section.el ends here
