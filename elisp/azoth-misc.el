;;; azoth-misc.el --- Some useful packages -*- lexical-binding: t; -*-
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
;;  Useful packages.
;;
;;; Code:

;;; Very useful libraries
(use-package f          ;; files
  :straight t
  :defer t)

(use-package dash       ;; lists
  :straight t
  :defer t)

(use-package ht         ;; hash-tables
  :straight t
  :defer t)

(use-package s          ;; strings
  :straight t
  :defer t)

(use-package a         ;; association lists
  :straight t
  :defer t)

(use-package anaphora  ;; anaphora
  :straight t
  :defer t)

;;; helpful
(use-package helpful
  :straight t
  :defer t
  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

;; recentf
(use-package recentf
  :straight t
  :defer t
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))

;;; undo tree
(use-package undo-tree
  :straight t
  :defer t
  :diminish undo-tree-mode
  :commands (undo-tree-undo global-undo-tree-mode)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

;;; markdown
(use-package markdown-mode
  :straight t
  :defer t)

;;; yaml-mode
(use-package yaml-mode
  :straight t
  :defer t)

(provide 'azoth-misc)
;;; azoth-misc.el ends here
