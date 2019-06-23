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
   (draft     :initform nil :initarg :draft)
   (message   :initform nil :initarg :text)
   (file      :initform nil :initarg :file)
   (range     :initform nil :initarg :range)
   (side      :initform nil :initarg :side))

  "Class that binds together all information related to the single comment.

AUTHOR  is a string representing commment author
DATE    is the encoded TIME representing comment post date
MESSAGE is a string representing actual comment message
FILE    is a string representing filename this comment refers to
RANGE   is an alist with 'start_line', 'start_col', 'end_line', 'end_col' keys
SIDE    is a boolean flag indicating that this comment is for the 'BASE' side")

;;; _
(provide 'magit-gerrit-comment)
;;; magit-gerrit-comment.el ends here
