;;; init.el --- Here am I. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 cervelle
;;
;; Author: cervelle <http://github.io/cervelleless>
;; Maintainer: cervelle <cervelleless@gmail.com>
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
;; 
;; gc.

(setq gc-cons-threshold 402653184)
(add-hook 'after-emacs-hook #'(lambda ()
                              ;; restore after startup
                              (setq gc-cons-threshold 16777216)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


(let (file-name-handler-alist)
  ;; Ensure Emacs is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; the path of core settings
(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))

(require 'azoth)

(provide 'init)
;;; init.el ends here
