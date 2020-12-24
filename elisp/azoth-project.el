;;; azoth-project.el --- project manager -*- lexical-binding: t; -*-
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
;;  Project manager.
;;
;;; Code:

(use-package projectile
  :straight t
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'hybrid
        projectile-ignored-projects '("~/" "/tmp")))


(provide 'azoth-project)
;;; azoth-project.el ends here
