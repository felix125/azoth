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
;; (require 'bind-key)

;;; TODO font and frame size
;; frame size
(setq initial-frame-alist
      '((top . 1) (left . 10) (width  . 280) (height . 150)))

;; font
(add-to-list 'default-frame-alist
             '(font . "M+ 1mn Light-18"))


;;; modus themes
(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-slanted-constructs t
	modus-themes-bold-constructs t
	modus-themes-fringes nil	; {nil,'subtle,'intense}
	modus-themes-mode-line '3d	; {nil,'3d,'moody}
	modus-themes-syntax nil ; Lots of options---continue reading the manual
	modus-themes-intense-hl-line nil
	modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
	modus-themes-links 'neutral-underline ; Lots of options---continue reading the manual
	modus-themes-no-mixed-fonts nil
	modus-themes-prompts nil	; {nil,'subtle,'intense}
	modus-themes-completions nil	; {nil,'moderate,'opinionated}
	modus-themes-region 'bg-only-no-extend ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
	modus-themes-diffs nil	; {nil,'desaturated,'fg-only,'bg-only}
	modus-themes-org-blocks nil	; {nil,'grayscale,'rainbow}
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
  :config
  (load-theme 'modus-vivendi t))




(provide 'azoth-ui)
;;; azoth-ui.el ends here
