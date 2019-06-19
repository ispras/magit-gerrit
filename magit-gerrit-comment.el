;;; magit-gerrit-comment.el --- magit-gerrit comment -*- lexical-binding: t; -*-
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

(defclass magit-gerrit--commentinfo ()
  ((author    :initform nil :initarg :author)
   (date      :initform nil :initarg :date)
   (message   :initform nil :initarg :text)
   (file      :initform nil :initarg :file)
   (range     :initform nil :initarg :range))

  "Class that binds together all information related to the single comment.

AUTHOR  is a string representing commment author
DATE    is the encoded TIME representing comment post date
MESSAGE is a string representing actual comment message
FILE    is a string representing filename this comment refers to
RANGE   is an alist with 'start_line', 'start_col', 'end_line', 'end_col' keys")

;;; _
(provide 'magit-gerrit-comment)
;;; magit-gerrit-comment.el ends here
