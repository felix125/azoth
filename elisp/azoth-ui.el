;;; azoth-ui.el --- ui with modus-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 cervelleless
;;
;; Author: cervelleless <http://github/felix>
;; Maintainer: cervelleless <cervelleless@gmail.com>
;; Created: December 02, 2020
;; Modified: December 02, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/felix/salt-ui
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  ui with doom-themes
;;
;;; Code:

(eval-when-compile
  (require 'use-package))

;;; Font and frame size
;; frame size
(setq initial-frame-alist
      '((top . 1) (left . 10) (width  . 280) (height . 150)))

;; font
(add-to-list 'default-frame-alist
             '(font . "M+ 1mn Light-18"))

;;; Remove menu-bar, scroll-bar and tool-bar.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;; display line number
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :hook (text-mode . display-line-numbers-mode))

;;; TODO: deal with word-wrap issue of language-mixed style.
;;; visual-fill-column
(use-package visual-fill-column
  :straight t
  :init
  (setq visual-fill-column-inhibit-sensible-window-split t
	visual-fill-column-width 80)
  :hook
  (text-mode . visual-line-mode)
  (prog-mode . visual-line-mode)
  (visual-line-mode . visual-fill-column-mode)
  :config
  (add-hook 'visual-line-mode-hook #'(lambda ()
				       (setq-local word-wrap nil)))
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;;; modus themes
(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-no-mixed-fonts t
	modus-themes-slanted-constructs t
	modus-themes-bold-constructs t
	modus-themes-fringes 'subtle	; {nil,'subtle,'intense}
	modus-themes-mode-line '3d	; {nil,'3d,'moody}
	modus-themes-syntax 'yellow-comments-green-strings ; Lots of options---continue reading the manual
	modus-themes-intense-hl-line nil
	modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
	modus-themes-links 'neutral-underline ; Lots of options---continue reading the manual
	modus-themes-prompts 'intense	      ; {nil,'subtle,'intense}
	modus-themes-completions 'moderate	; {nil,'moderate,'opinionated}
	modus-themes-region nil ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
	modus-themes-diffs nil	; {nil,'desaturated,'fg-only,'bg-only}
	modus-themes-org-blocks 'grayscale	; {nil,'grayscale,'rainbow}
	modus-themes-headings ; Lots of options---continue reading the manual
	'((1 . section)
	  (2 . section-no-bold)
	  (3 . rainbow-line)
	  (t . rainbow-line-no-bold))
	modus-themes-variable-pitch-headings nil
	modus-themes-scale-headings nil
	modus-themes-scale-1 1.1
	modus-themes-scale-2 1.15
	modus-themes-scale-3 1.21
	modus-themes-scale-4 1.27
	modus-themes-scale-5 1.33)
  (set-face-attribute 'default nil :family "M+ 1mn Light" :height 180)
  (set-face-attribute 'variable-pitch nil :family "M+ 1mn Light" :height 180)
  (set-face-attribute 'variable-pitch nil :family "M+ 1mn Light" :height 180)
  :config
  (load-theme 'modus-vivendi t))




(provide 'azoth-ui)
;;; azoth-ui.el ends here
