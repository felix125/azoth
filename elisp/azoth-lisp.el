;;; azoth-lisp.el --- coding enviroment for guile elisp scheme -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 cervelleless
;;
;; Author: cervelleless <http://github.com/cervelleless>
;; Maintainer: cervelleless <cervelleless@gmail.com>
;; Created: December 12, 2020
;; Modified: December 12, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/felix/salt-lisps
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Coding enviroment for guile elisp scheme and so on.
;;
;;; Code:

;;; lispy
(use-package lispy
  :straight t
  :defer t
  :init
  (setq show-paren-delay 0
        show-paren-style 'parenthesis)
  :hook
  (emacs-lisp-mode . lispy-mode)
  (scheme-mode . lispy-mode)
  (racket-mode . lispy-mode)
  (lispy-mode . show-paren-mode))

;; elisp-slime-nav
(use-package elisp-slime-nav
  :straight t
  :defer t
  :hook
  (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package geiser
  :straight t
  :defer t)

;; racket-mode
(use-package racket-mode
  :straight t
  :defer t
  :hook
  (racket-mode . lispy-mode)
  (racket-mode . racket-xp-mode))



(provide 'azoth-lisp)
;;; azoth-lisp.el ends here
