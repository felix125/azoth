;;; azoth.el --- Here am I. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 cervelle
;;
;; Author: Cervelleless <http://github.io/cervelleless>
;; Maintainer: Cervelle <cervelleless@gmail.com>
;; Created: November 17, 2020
;; Modified: November 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage:
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Here am I.
;;
;;; Code:

(require 'azoth-repo)
(require 'azoth-env)
(require 'azoth-completion)
(require 'azoth-project)
(require 'azoth-misc)
(require 'azoth-ui)
(require 'azoth-magit)
(require 'sulfur)
(require 'mercury)
(require 'azoth-lisp)
(require 'azoth-eshell)
(require 'azoth-web)

(provide 'azoth)
;;; azoth.el ends here
