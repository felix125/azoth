;;; azoth-completion.el --- selectrum framework -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Cervelleless
;;
;; Author: Cervelleless <http://github.com/cervelleless>
;; Maintainer: Cervelleless <cervelleless@gmail.com>
;; Created: December 02, 2020
;; Modified: December 02, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage:
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  selectrum, consult, embark, marginalia and prescient.
;;
;;; Code:

;;; selectrum
(use-package selectrum
  :straight t
  :hook (after-init . selectrum-mode))

;;; consult
(use-package consult
  :straight t)

;;; selectrum-prescient
(use-package selectrum-prescient
  :straight t
  :hook (selectrum-mode . prescient-persist-mode)
        (selectrum-mode . selectrum-prescient-mode))
        
;;; consult-selectrum
(use-package consult-selectrum
  :straight t)

;;; embark
(use-package embark
  :straight t)

;;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action!
  :bind (:map embark-general-map
              ("A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  ;; (marginalia-mode)
  ;; When using Selectrum and `marginalia-cycle' as an Embark action,
  ;; ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :hook (selectrum-mode . marginalia-mode))
(provide 'azoth-completion)
;;; azoth-completion.el ends here
