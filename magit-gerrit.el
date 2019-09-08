;;; magit-gerrit.el --- Magit plugin for Gerrit  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2013-2019 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;; Maintainer:
;;    Valeriy Savchenko     <vsavchenko@ispras.ru>
;;    Konstantin Sorokin    <ksorokin@ispras.ru>
;;    Georgiy Pankratenko   <gpankratenko@ipsras.ru>

;; Keywords: git tools vc gerrit
;; URL: https://github.com/ispras/magit-gerrit
;; Package-Requires: ((magit "2.3.1"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Magit plugin to make Gerrit code review easy-to-use from emacs and
;; without the need for a browser!
;;
;; Currently uses the [deprecated] gerrit ssh interface, which has
;; meant that obtaining the list of reviewers is not possible, only
;; the list of approvals (those who have already verified and/or code
;; reviewed).
;;
;;; To Use:
;;
;; (require 'magit-gerrit)
;; (setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")
;;
;;
;; M-x `magit-status'
;; h R  <= magit-gerrit uses the R prefix, see help
;;
;;; Workflow:
;;
;; 1) *check out branch => changes => (ma)git commit*
;; 2) R P  <= [ger*R*it *P*ush for review]
;; 3) R A  <= [ger*R*it *A*dd reviewer] (by email address)
;; 4) *wait for verification/code reviews* [approvals shown in status]
;; 5) R S  <= [ger*R*it *S*ubmit review]
;;
;;; Other Comments:
;; `magit-gerrit-ssh-creds' is buffer local, so if you work with
;; multiple Gerrit's, you can make this a file or directory local
;; variable for one particular project.
;;
;; If your git remote for gerrit is not the default "origin", then
;; `magit-gerrit-remote' should be adjusted accordingly (e.g. "gerrit")
;;
;; Recommended to auto add reviewers via git hooks (precommit), rather
;; than manually performing 'R A' for every review.
;;
;; `magit-gerrit' will be enabled automatically on `magit-status' if
;; the git remote repo uses the same creds found in
;; `magit-gerrit-ssh-creds'.
;;
;; Ex:  magit-gerrit-ssh-creds == br.fransioli@gerrit.org
;; $ cd ~/elisp; git remote -v => https://github.com/terranpro/magit-gerrit.git
;; ^~~ `magit-gerrit-mode' would *NOT* be enabled here
;;
;; $ cd ~/gerrit/prja; git remote -v => ssh://br.fransioli@gerrit.org/.../prja
;; ^~~ `magit-gerrit-mode' *WOULD* be enabled here
;;
;;; Code:

(require 'magit)
(if (locate-library "magit-popup")
    (require 'magit-popup))
(require 'json)
(require 'dash)

(eval-when-compile
  (require 'cl-lib))

(require 'magit-gerrit-comment-ui)
(require 'magit-gerrit-requests)

;; Define a defvar-local macro for Emacs < 24.3
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

(defvar-local magit-gerrit-ssh-creds nil
  "Credentials used to execute gerrit commands via ssh of the form ID@Server")

(defvar-local magit-gerrit-remote "origin"
  "Default remote name to use for gerrit (e.g. \"origin\", \"gerrit\")")

(defcustom magit-gerrit-url "http://gerrit"
  "URL for the gerrit web view"
  :group 'magit-gerrit
  :type 'string)

(defcustom magit-gerrit-popup-prefix "R"
  "Key code to open magit-gerrit popup"
  :group 'magit-gerrit
  :type 'key-sequence)

(defcustom magit-gerrit-jump-to-reviews-key "jr"
  "Key code to jump to magit-gerrit reviews section"
  :group 'magit-gerrit
  :type 'key-sequence)

(defun gerrit-command (cmd &rest args)
  (let ((gcmd (concat
               "-x -p 29418 "
               (or magit-gerrit-ssh-creds
                   (error "`magit-gerrit-ssh-creds' must be set!"))
               " "
               "gerrit "
               cmd
               " "
               (mapconcat 'identity args " "))))
    ;; (message (format "Using cmd: %s" gcmd))
    gcmd))

(defun gerrit-query (prj &optional status)
  (gerrit-command "query"
                  "--format=JSON"
                  "--all-approvals"
                  "--comments"
                  "--current-patch-set"
                  (concat "project:" prj)
                  (concat "status:" (or status "open"))))

(defun gerrit-ssh-cmd (cmd &rest args)
  (apply #'call-process
         "ssh" nil nil nil
         (split-string (apply #'gerrit-command cmd args))))

(defun gerrit-review-abandon (prj rev)
  (gerrit-ssh-cmd "review" "--project" prj "--abandon" rev))

(defun gerrit-review-submit (prj rev &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--submit"
                  (if msg msg "") rev))

(defun gerrit-code-review (prj rev score &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--code-review" score
                  (if msg msg "") rev))

(defun gerrit-review-verify (prj rev score &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--verified" score
                  (if msg msg "") rev))

(defun magit-gerrit-get-remote-url ()
  (magit-git-string "ls-remote" "--get-url" magit-gerrit-remote))

(defun magit-gerrit-get-project ()
  (let* ((regx (rx (zero-or-one ?:) (zero-or-more (any digit)) ?/
                   (group (not (any "/")))
                   (group (one-or-more (not (any "."))))))
         (str (or (magit-gerrit-get-remote-url) ""))
         (sstr (car (last (split-string str "//")))))
    (when (string-match regx sstr)
      (concat (match-string 1 sstr)
              (match-string 2 sstr)))))

(defun magit-gerrit-string-trunc (str maxlen)
  (if (> (length str) maxlen)
      (concat (substring str 0 maxlen)
              "...")
    str))

(defun magit-gerrit-create-branch-force (branch parent)
  "Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
Succeed even if branch already exist
\('git checkout -B BRANCH REVISION')."
  (cond ((run-hook-with-args-until-success
          'magit-create-branch-hook branch parent))
        ((and branch (not (string= branch "")))
         (magit-save-repository-buffers)
         (magit-run-git "checkout" "-B" branch parent))))


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

(defun magit-gerrit-remote-update (&optional remote)
  nil)

(defun magit-gerrit-review-at-point ()
  (get-text-property (point) 'magit-gerrit-jobj))

(defsubst magit-gerrit-process-wait ()
  (while (and magit-this-process
              (eq (process-status magit-this-process) 'run))
    (sleep-for 0.005)))

(defun magit-gerrit--fetch-patchset (ref)
  (let* ((magit-proc (magit-fetch-other magit-gerrit-remote ref)))
    (message (format "Waiting a git fetch from %s to complete..."
                     magit-gerrit-remote))
    (magit-gerrit-process-wait)
    (magit-rev-hash "FETCH_HEAD")))

(defun magit-gerrit--fetch-patchset-by-index (jobj index)
  "Get the patchset reference from JOBJ by INDEX, fetch it and return its hash."
  (pcase index
    ("last" (magit-gerrit--fetch-patchset
             (alist-get 'ref (alist-get 'currentPatchSet jobj))))
    ;; here we have an indirect requirement that at least fetch already happened
    ;; as base is fetched together with the main commit
    ("base" (propertize (magit-rev-hash "FETCH_HEAD~1") 'base 't))
    (index  (magit-gerrit--fetch-patchset
             (alist-get 'ref (elt (alist-get 'patchSets jobj)
                                  (1- (string-to-number index))))))))

(defun magit-gerrit--fetch-patchset-range (origin changed)
  "Download a Gerrit Review Patchset for diff viewing"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((changed-hash (magit-gerrit--fetch-patchset-by-index jobj changed))
            (origin-hash  (magit-gerrit--fetch-patchset-by-index jobj origin)))
        (concat origin-hash ".." changed-hash)))))

(defun magit-gerrit--view-patchset-impl (args viewer)
  "View selected patchset with a given viewer

`VIEWER' should accept one argument: revision range"
  (-let* (((origin changed) args)
          (revision-range (magit-gerrit--fetch-patchset-range origin changed)))
    (when revision-range
      (apply viewer (list revision-range)))))

(define-suffix-command magit-gerrit-view-patchset-diff (args)
  "View the Diff for a Patchset"
  (interactive (list (magit-gerrit-view-arguments)))
  (magit-gerrit--view-patchset-impl args 'magit-diff-range))

(defun magit-gerrit--kill-ediff-buffer (buffer)
  "Kill BUFFER if this buffer is a temporary git buffer."
  (when (not (buffer-file-name buffer))
    (ediff-kill-buffer-carefully buffer)))

(defun magit-gerrit--close-ediff ()
  ;; kill buffers A and B without bothering the user...
  (magit-gerrit--kill-ediff-buffer ediff-buffer-A)
  (magit-gerrit--kill-ediff-buffer ediff-buffer-B)
  ;; ...and quit from the current ediff session
  (ediff-cleanup-mess))

(defun magit-gerrit--ediff-set-bindings (jobj revA revB files index comments)
  (let* ((goto-index
          (lambda (new-index)
            (magit-gerrit--close-ediff)
            ;; we consider `NEW-INDEX' to be correct and create a new
            ;; ediff session for it
            (magit-gerrit--ediff-compare jobj revA revB files new-index
                                         comments)))
         ;; this is an actual callback generator that checks index
         ;; boundaries and outputs the given error message if the check
         ;; has failed
         (command (lambda (new-index error-message)
                    (lambda ()
                      (interactive)
                      (if (and (>= new-index 0) (< new-index (length files)))
                          (funcall goto-index new-index)
                        (message error-message))))))

    ;; TODO: get keys from the keymap
    (define-key ediff-mode-map "N"
      (funcall command (1+ index) "There is no next file"))

    (define-key ediff-mode-map "P"
      (funcall command (1- index) "There is no previous file"))))

(defun magit-gerrit--ediff-compare (jobj revA revB files index comments-drafts)
  "Compare REVA:FILES[INDEX] with REVB:FILES[INDEX] using Ediff.

FILES have to be relative to the top directory of the
repository.
COMMENTS is a cons of comments for both revisions

It is a tweaked copy-paste of `MAGIT-EDIFF-COMPARE'."
  (magit-with-toplevel
    (-let* ((conf (current-window-configuration))
            (file (nth index files))
            (changeset (number-to-string (alist-get 'number jobj)))
            (binding-setter
             (lambda ()
               (magit-gerrit--ediff-set-bindings jobj revA revB files index
                                                 comments-drafts)))
            (bufA (magit-find-file revA file))
            (bufB (magit-find-file revB file))
            (get-file-comments
             (lambda (comments-alist)
               (alist-get file comments-alist nil nil #'string=)))
            (get-revision-comments
             (-lambda ((comments drafts))
               (append
                (funcall get-file-comments comments)
                (funcall get-file-comments drafts))))
            ((commentsA commentsB)
             (seq-map
              (lambda (revision-comments-drafts)
                (funcall get-revision-comments revision-comments-drafts))
              comments-drafts))
            (comments-adder
             (lambda ()
               (-each `((,revA ,commentsA ,bufA)
                        (,revB ,commentsB ,bufB))
                 (-lambda ((revision comments buffer))
                   (magit-gerrit--add-comments revision comments buffer)))))
            (propagate-review-alist
             (lambda ()
               (-each `((,bufA ,revA)
                        (,bufB ,revB))
                 (-lambda ((buffer revision))
                   (with-current-buffer buffer
                     ;; we use REVISION (and CHANGESET) to post comments added
                     ;; in revision's buffer that's why
                     ;; for the BASE buffer on the left
                     ;; set revision to the one of the right side
                     (magit-gerrit-ui-mode t)
                     (setq-local magit-gerrit-review-alist
                                 `((changeset . ,changeset)
                                   (revision . ,(if (get-text-property 0 'base
                                                                       revision)
                                                    (propertize revB 'base 't)
                                                  revision))))))))))
      (ediff-buffers
       bufA bufB
       `((lambda ()
           (setq-local
            ediff-quit-hook
            (lambda ()
              (magit-gerrit--close-ediff)
              (let ((magit-ediff-previous-winconf ,conf))
                (run-hooks 'magit-ediff-quit-hook)))))
         ,propagate-review-alist
         ,binding-setter
         ,comments-adder)
       'ediff-revision))))

(defun magit-gerrit--fetch-revision-comments (revision)
  (let* ((jobj (magit-gerrit-review-at-point))
         (changeset (number-to-string (alist-get 'number jobj)))
         (base-url
          (magit-gerrit--patchset-url changeset revision))
         (comments-url
          (concat base-url "/comments"))
         (drafts-url
          (concat base-url "/drafts"))
         (fetch-alist
          `((comments . ((url . ,(concat base-url "/comments"))
                         (parse-params . nil)))
            (drafts . ((url . ,(concat base-url "/drafts"))
                       (parse-params . (t)))))))
    (seq-map
     (-lambda ((comment-type . params))
       (-let (((&alist 'parse-params 'url) params))
         (magit-gerrit--parse-comments (magit-gerrit--get url) parse-params)))
     fetch-alist)))

(defun magit-gerrit--fetch-comments (origin changed)
  (let* ((origin
          ;; "BASE" ref comments always relate to some patchset
          ;; we fetch them from this patchsets' overall comments array
          ;; in current case, needed patchsets' ref is CHANGED
          (if (get-text-property 0 'base origin)
              changed
            origin))
         (refs (list origin changed)))
    (seq-map
     (lambda (ref)
       (magit-gerrit--fetch-revision-comments ref))
     refs)))

(defun magit-gerrit--view-ediff (compare revision-range)
  (-let* ((jobj (magit-gerrit-review-at-point))
          (files (magit-changed-files revision-range))
          ((origin . changed) (magit-split-range revision-range))
          (comments (magit-gerrit--fetch-comments origin changed)))
    ;; start with the first file of the patchset
    ;; TODO: change to 'commit message' in the future
    (funcall compare jobj origin changed files 0 comments)))

(defun magit-gerrit--view-patchset-in-ediff-impl (args compare)
  (magit-gerrit--view-patchset-impl
   args (lambda (range) (funcall #'magit-gerrit--view-ediff compare range))))

(define-suffix-command magit-gerrit-view-patchset-ediff (args)
  "View the Diff for a Patchset in Ediff"
  (interactive (list (magit-gerrit-view-arguments)))
  (magit-gerrit--view-patchset-in-ediff-impl
   args #'magit-gerrit--ediff-compare))

(defun magit-gerrit--resolve (jobj origin changed files index comments)
  (magit-gerrit--ediff-compare jobj changed "{worktree}" files 0
                               ;; dismiss comments for the "right" part
                               ;; as it is a worktree
                               (-replace-at 1 nil comments)))

(define-suffix-command magit-gerrit-resolve-patchset (args)
  "Resolve Patchset in Ediff"
  (interactive (list (magit-gerrit-view-arguments)))
  (magit-gerrit--view-patchset-in-ediff-impl
   (-replace-at 0 "base" args) #'magit-gerrit--resolve))

(defun magit-gerrit-download-patchset ()
  "Download a Gerrit Review Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (magit-gerrit--fetch-patchset-by-index jobj "last"))))

(defun magit-gerrit-download-and-checkout-patchset ()
  "Download and checkout a Gerrit Review Patchset"
  (interactive)
  (magit-gerrit-download-patchset)
  (magit-gerrit-create-branch-force branch "FETCH_HEAD"))

(defun magit-gerrit-cherry-pick-patchset ()
  "Cherry-pick a Gerrit Review Patchset"
  (interactive)
  (magit-gerrit-download-patchset)
  (magit--cherry-pick '("FETCH_HEAD") nil))

(defun magit-gerrit-browse-review ()
  "Browse the Gerrit Review with a browser."
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (browse-url (alist-get 'url jobj)))))

(defun magit-gerrit--copy-review-impl (with-commit-message)
  "Copy review url and commit message."
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (with-temp-buffer
        (insert
         (concat (cdr (assoc 'url jobj))
                 (when with-commit-message
                   (concat " "
                           (car (split-string
                                 (cdr (assoc 'commitMessage jobj))
                                 "\n" t))))))
        (clipboard-kill-region (point-min) (point-max))))))

(defun magit-gerrit-copy-review-url ()
  "Copy review url only"
  (interactive)
  (magit-gerrit--copy-review-impl nil))

(defun magit-gerrit-copy-review-url-commit-message ()
  "Copy review url with commit message"
  (interactive)
  (magit-gerrit--copy-review-impl t))

(magit-define-section-jumper magit-gerrit-jump-to-reviews
  "Reviews" gerrit-reviews)

(defun magit-insert-gerrit-reviews ()
  (let ((magit-git-executable "ssh")
        (magit-git-global-arguments nil)
        (command
         (split-string (gerrit-query (magit-gerrit-get-project)))))
    (magit-insert-section (gerrit-reviews)
      (magit-insert-heading "Reviews:")
      (magit-git-wash #'magit-gerrit-wash-reviews command)
      (insert "\n"))))

(defun magit-gerrit-add-reviewer ()
  (interactive)
  "ssh -x -p 29418 user@gerrit gerrit set-reviewers --project toplvlroot/prjname --add email@addr"

  (gerrit-ssh-cmd "set-reviewers"
                  "--project" (magit-gerrit-get-project)
                  "--add" (read-string "Reviewer Name/Email: ")
                  (cdr-safe (assoc 'id (magit-gerrit-review-at-point)))))

(defun magit-gerrit--score (post-function args)
  "Score review at point and post the score using POST-FUNCTION"
  (let ((score (completing-read "Score: "
                                '("-2" "-1" "0" "+1" "+2")
                                nil t
                                "+1"))
        (rev (cdr-safe (assoc
                        'revision
                        (cdr-safe (assoc 'currentPatchSet
                                         (magit-gerrit-review-at-point))))))
        (prj (magit-gerrit-get-project)))
    (funcall post-function prj rev score args)
    (magit-refresh)))

(defun magit-gerrit-verify-review (args)
  "Verify a Gerrit Review"
  (interactive (magit-gerrit-arguments))
  (magit-gerrit--score #'gerrit-review-verify args))

(defun magit-gerrit-code-review (args)
  "Perform a Gerrit Code Review"
  (interactive (magit-gerrit-arguments))
  (-let* (((message score) args)
          (jobj (magit-gerrit-review-at-point))
          (changeset (number-to-string (alist-get 'number jobj)))
          (revision (number-to-string
                     (alist-get 'number (alist-get 'currentPatchSet jobj))))
          (url (concat (magit-gerrit--patchset-url changeset revision)
                       "/review"))
          (data `(
                  ("message" . ,message)
                  ("labels" . (("Code-Review" . ,(string-to-number score))))
                  ;; Publish drafts for the current revision
                  ("drafts" . "PUBLISH"))))
    (magit-gerrit--post url data "POST")))

(defun magit-gerrit-submit-review (args)
  "Submit a Gerrit Code Review"
  ;; "ssh -x -p 29418 user@gerrit gerrit review REVISION  -- --project PRJ --submit "
  (interactive (magit-gerrit-arguments))
  (gerrit-ssh-cmd "review"
                  (cdr-safe (assoc
                             'revision
                             (cdr-safe (assoc 'currentPatchSet
                                              (magit-gerrit-review-at-point)))))
                  "--project"
                  (magit-gerrit-get-project)
                  "--submit"
                  args)
  (magit-fetch-from-upstream magit-gerrit-remote ""))

(defun magit-gerrit-push-review (status)
  (let* ((branch (or (magit-get-current-branch)
                     (error "Don't push a detached head.  That's gross")))
         (commitid (or (when (eq (oref (magit-current-section) type)
                                 'commit)
                         (oref (magit-current-section) value))
                       (error "Couldn't find a commit at point")))
         (rev (magit-rev-parse (or commitid
                                   (error "Select a commit for review"))))

         (branch-remote (and branch (magit-get "branch" branch "remote"))))

    ;; (message "Args: %s "
    ;;     (concat rev ":" branch-pub))

    (let*
        ((branch-merge
          (if (or (null branch-remote)
                  (string= branch-remote "."))
              (completing-read
               "Remote Branch: "
               (let ((rbs (magit-list-remote-branch-names)))
                 (mapcar
                  #'(lambda (rb)
                      (and (string-match (rx bos
                                             (one-or-more (not (any "/")))
                                             "/"
                                             (group (one-or-more any))
                                             eos)
                                         rb)
                           (concat "refs/heads/" (match-string 1 rb))))
                  rbs)))
            (and branch (magit-get "branch" branch "merge"))))
         (branch-pub
          (progn
            (string-match (rx "refs/heads" (group (one-or-more any)))
                          branch-merge)
            (format "refs/%s%s/%s"
                    status (match-string 1 branch-merge) branch))))


      (when (or (null branch-remote)
                (string= branch-remote "."))
        (setq branch-remote magit-gerrit-remote))

      (magit-run-git-async "push" "-v" branch-remote
                           (concat rev ":" branch-pub)))))

(defun magit-gerrit-create-review ()
  (interactive)
  (magit-gerrit-push-review 'for))

(defun magit-gerrit-create-draft ()
  (interactive)
  (magit-gerrit-push-review 'drafts))

(defun magit-gerrit-publish-draft ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
        (id (cdr-safe (assoc 'id
                             (magit-gerrit-review-at-point))))
        (rev (cdr-safe (assoc
                        'revision
                        (cdr-safe (assoc 'currentPatchSet
                                         (magit-gerrit-review-at-point)))))))
    (gerrit-ssh-cmd "review" "--project" prj "--publish" rev))
  (magit-refresh))

(defun magit-gerrit-delete-draft ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
        (id (cdr-safe (assoc 'id
                             (magit-gerrit-review-at-point))))
        (rev (cdr-safe (assoc
                        'revision
                        (cdr-safe (assoc 'currentPatchSet
                                         (magit-gerrit-review-at-point)))))))
    (gerrit-ssh-cmd "review" "--project" prj "--delete" rev))
  (magit-refresh))

(defun magit-gerrit-abandon-review ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
        (id (cdr-safe (assoc 'id
                             (magit-gerrit-review-at-point))))
        (rev (cdr-safe (assoc
                        'revision
                        (cdr-safe (assoc 'currentPatchSet
                                         (magit-gerrit-review-at-point)))))))
    ;; (message "Prj: %s Rev: %s Id: %s" prj rev id)
    (gerrit-review-abandon prj rev)
    (magit-refresh)))

(defun magit-gerrit-create-branch (branch parent))

(define-transient-command magit-gerrit ()
  "Invoke a Magit Gerrit command from a list of available commands."
  ["Actions"
   [("P" "Push Commit For Review" magit-gerrit-create-review)
    ("W" "Push Commit For Draft Review" magit-gerrit-create-draft)
    ("p" "Publish Draft Patchset" magit-gerrit-publish-draft)
    ("k" "Delete Draft" magit-gerrit-delete-draft)
    ("A" "Add Reviewer" magit-gerrit-add-reviewer)
    ("V" "Verify" magit-gerrit-verify-review)
    ("C" "Code Review" magit-gerrit-code-review)
    ("c" "Copy Review" magit-gerrit-copy-review)]
   [("v" "View Patchset" magit-gerrit-view)
    ("D" "Download Patchset" magit-gerrit-download-and-checkout-patchset)
    ("S" "Submit Review" magit-gerrit-submit-review)
    ("B" "Abandon Review" magit-gerrit-abandon-review)
    ("b" "Browse Review" magit-gerrit-browse-review)
    ("H" "Cherry-pick Patchset" magit-gerrit-cherry-pick-patchset)]]
  ["Options"
   (magit-gerrit:-message)
   (magit-gerrit:-score)]
  (interactive)
  (transient-setup 'magit-gerrit nil nil))

(defun magit-gerrit-arguments ()
  (list (transient-args 'magit-gerrit)))

;; Attach Magit Gerrit to Magit's default help popup
;; See: https://github.com/magit/magit/wiki/Converting-popup-modifications-to-transient-modifications#adding-an-action
(transient-append-suffix 'magit-dispatch
  "r" `(,magit-gerrit-popup-prefix "Gerrit" magit-gerrit))

(define-transient-command magit-gerrit-view ()
  "Popup for Magit Gerrit interface for viewing diff for patchsets"
  ["Options"
   (magit-gerrit-view:-origin)
   (magit-gerrit-view:-changed)]
  ["Actions"
   ("d" "Diff" magit-gerrit-view-patchset-diff)
   ("e" "Ediff" magit-gerrit-view-patchset-ediff)
   ("r" "Resolve" magit-gerrit-resolve-patchset)]
  (interactive)
  (transient-setup 'magit-gerrit-view nil nil))

(defun magit-gerrit-view-arguments ()
  (transient-args 'magit-gerrit-view))

(defclass magit-gerrit--patchset (transient-option)
  ((default :initarg :default)))

(defclass magit-gerrit--review (transient-option)
  ((default :initarg :default)))

(cl-defmethod transient-init-value ((obj magit-gerrit--patchset))
  ;; take init value with default
  (oset obj value (oref obj default)))

(cl-defmethod transient-infix-value ((obj magit-gerrit--patchset))
  ;; no trickery: just return the value
  (oref obj value))

(cl-defmethod transient-infix-read :before ((obj magit-gerrit--patchset))
  ;; By defaul, when you choose to change the value of an argument that
  ;; already has the value, it simply sets the value to nil. Patchsets
  ;; should never be nil. In order to resolve this, we set it to nil
  ;; ourselves first and delegate to the parent implementation after that.
  ;;
  ;; Default implementation for infix-read does a lot of different useful
  ;; things. That's why we use :before mark to tell Emacs to call base class
  ;; method after this implementation.
  (with-slots (value) obj
    (when value
      (oset obj value nil))))

(define-infix-argument magit-gerrit:-message ()
  :description "Commit message"
  :class 'magit-gerrit--patchset
  :key "m"
  :argument "message="
  :reader 'magit-gerrit--read-message
  :default "")

(define-infix-argument magit-gerrit:-score ()
  :description "Review score"
  :class 'magit-gerrit--patchset
  :key "s"
  :argument "score="
  :reader 'magit-gerrit--read-score
  :default "0")

(define-infix-argument magit-gerrit-view:-origin ()
  :description "Compare change to"
  :class 'magit-gerrit--patchset
  :key "o"
  :argument "origin="
  :reader 'magit-gerrit--read-patchset-base
  :default "base")

(define-infix-argument magit-gerrit-view:-changed ()
  :description "Patchset to view"
  :class 'magit-gerrit--patchset
  :key "p"
  :argument "patchset="
  :reader 'magit-gerrit--read-patchset
  :default "last")

(defun magit-gerrit--read-message
    (prompt initial-input history)
  (completing-read prompt nil nil nil initial-input history))

(defun magit-gerrit--read-score
    (prompt initial-input history)
  (completing-read prompt '("-2" "-1" "0" "+1" "+2") nil t initial-input))

(defun magit-gerrit--read-patchset
    (prompt initial-input history &optional include-base)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let* ((overall-number
              (alist-get 'number (alist-get 'currentPatchSet jobj)))
             ;; all patchset IDs
             (numerical (mapcar #'number-to-string
                                (number-sequence 1 overall-number)))
             (changed-candidates (cons "last" numerical))
             ;; all candidates for completion
             (candidates (if include-base
                             (cons "base" changed-candidates)
                           changed-candidates)))
        (completing-read prompt candidates nil t nil nil)))))

(defun magit-gerrit--read-patchset-base (prompt initial-input history)
  (magit-gerrit--read-patchset prompt initial-input history t))

(define-transient-command magit-gerrit-copy-review ()
  "Popup console for copy review to clipboard."
  ["Actions"
   ("C" "url and commit message" magit-gerrit-copy-review-url-commit-message)
   ("c" "url only" magit-gerrit-copy-review-url)])

(defun magit-gerrit--set-default-gerrit-bindings (map)
  (define-key map magit-gerrit-popup-prefix 'magit-gerrit)
  (define-key map magit-gerrit-jump-to-reviews-key
    'magit-gerrit-jump-to-reviews))

(defvar magit-gerrit-mode-map
  (let ((map (make-sparse-keymap)))
    (magit-gerrit--set-default-gerrit-bindings map)
    map))

(define-minor-mode magit-gerrit-mode "Gerrit support for Magit"
  :lighter " Gerrit" :require 'magit-topgit :keymap 'magit-gerrit-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (or magit-gerrit-ssh-creds
      (error "You *must* set `magit-gerrit-ssh-creds' to enable magit-gerrit-mode"))
  (or (magit-gerrit-get-remote-url)
      (error "You *must* set `magit-gerrit-remote' to a valid Gerrit remote"))
  (cond
   (magit-gerrit-mode
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-gerrit-reviews
                            'magit-insert-stashes t t)
    (add-hook 'magit-create-branch-command-hook
              'magit-gerrit-create-branch nil t)
    ;; (add-hook 'magit-pull-command-hook 'magit-gerrit-pull nil t)
    (add-hook 'magit-remote-update-command-hook
              'magit-gerrit-remote-update nil t)
    (add-hook 'magit-push-command-hook
              'magit-gerrit-push nil t))

   (t
    (remove-hook 'magit-after-insert-stashes-hook
                 'magit-insert-gerrit-reviews t)
    (remove-hook 'magit-create-branch-command-hook
                 'magit-gerrit-create-branch t)
    ;; (remove-hook 'magit-pull-command-hook 'magit-gerrit-pull t)
    (remove-hook 'magit-remote-update-command-hook
                 'magit-gerrit-remote-update t)
    (remove-hook 'magit-push-command-hook
                 'magit-gerrit-push t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(defun magit-gerrit-detect-ssh-creds (remote-url)
  "Derive magit-gerrit-ssh-creds from remote-url.
Assumes remote-url is a gerrit repo if scheme is ssh
and port is the default gerrit ssh port."
  (let ((url (url-generic-parse-url remote-url)))
    (when (and (string= "ssh" (url-type url))
               (eq 29418 (url-port url)))
      (set (make-local-variable 'magit-gerrit-ssh-creds)
           (format "%s@%s" (url-user url) (url-host url)))
      (message "Detected magit-gerrit-ssh-creds=%s" magit-gerrit-ssh-creds))))

(defun magit-gerrit-check-enable ()
  (let ((remote-url (magit-gerrit-get-remote-url)))
    (when (and remote-url
               (or magit-gerrit-ssh-creds
                   (magit-gerrit-detect-ssh-creds remote-url))
               (string-match magit-gerrit-ssh-creds remote-url))
      ;; update keymap with prefix incase it has changed
      (magit-gerrit--set-default-gerrit-bindings magit-gerrit-mode-map)
      (magit-gerrit-mode t))))

;; Hack in dir-local variables that might be set for magit gerrit
(add-hook 'magit-status-mode-hook #'hack-dir-local-variables-non-file-buffer t)

;; Try to auto enable magit-gerrit in the magit-status buffer
(add-hook 'magit-status-mode-hook #'magit-gerrit-check-enable t)
(add-hook 'magit-log-mode-hook #'magit-gerrit-check-enable t)

(defvar magit-gerrit-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ]") 'magit-gerrit-next-comment)
    (define-key map (kbd "C-c [") 'magit-gerrit-prev-comment)
    (define-key map (kbd "C-c a") 'magit-gerrit-add-comment)
    map)
  "Keymap for `magit-gerrit-mode'.")

(define-minor-mode magit-gerrit-ui-mode
  "Minor mode for gerrit comment UI.

Enables special bindings for working with gerrit commits"
  :init-value nil
  :lighter " Gerrit"
  :group 'magit-gerrit-group)

(provide 'magit-gerrit)

;;; magit-gerrit.el ends here
