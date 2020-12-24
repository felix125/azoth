;;; mercury.el --- leader man -*- lexical-binding: t; -*-
;
;; Copyright (C) 2020 cervelleless
;;
;; Author: cervelleless <http://github.com/cervelleless>
;; Maintainer: cervelleless <cervelleless@gmail.com>
;; Created: December 02, 2020
;; Modified: December 02, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/cervelleless/mercury
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  leader man
;;
;;; Code:

;;; leader
(use-package hydra
  :straight t
  :defer t)

(defhydra mercury (:color blue :columns 5)
  "Launch!"
  ("b" mercury-buffers/body "+buffers")
  ("f" mercury-files/body "+files")
  ("h" mercury-help/body "+help & info")
  ("p" mercury-project/body "+projects")
  ("s" mercury-search/body "+search")
  ("t" mercury-toggles/body "+toggles")
  ("q" nil "cancel")
  ("SPC" execute-extended-command "M-x")
  ("<escape>" enter-sulfur-cmd-mode "Exit"))

;; Help & Info
(defhydra mercury-help (:color blue :columns 5)
  "Help & Info"
  ("f" helpful-callable "Describe function")
  ("F" helpful-function "Describe function")
  ("c" helpful-command "Describe interactive functions")
  ("k" helpful-key "Describe key")
  ("m" describe-mode "Describe mode")
  ("v" helpful-variable "Describe variable")
  ("<escape>" enter-sulfur-cmd-mode "Exit"))

;; Help & Info
(defhydra mercury-search (:color blue :columns 5)
  "Search"
  ("a" anzu-query-replace "Anzu Query-Replace")
  ("b" consult-line "Search in current buffer")
  ("p" projectile-rg "Search the current project with rg")
  ("<escape>" enter-sulfur-cmd-mode "Exit"))

;; buffers
(defhydra mercury-buffers (:color blue :columns 5)
  "Buffers & Bookmarks"
  ("c" consult-bookmark "consult-bookmark")
  ("i" ibuffer "ibuffer")
  ("n" next-buffer "Next buffer")
  ("p" previous-buffer "Previous buffer")
  ("s" basic-save-buffer "Save buffer")
  ("<escape>" enter-sulfur-cmd-mode "Exit"))

;; file
(defhydra mercury-files (:color blue :columns 5 :hint nil)
  "
+file:
  _f_: Find file        _d_: deer        _q_: Previous page
  _r_: Recent file      _s_: Save file"
  ("f" find-file)
  ("d" deer)
  ("q" mercury/body)
  ("r" consult-recent-file)
  ("s" save-buffer)
  ("<escape>" (enter-sulfur-cmd-mode) "Exit"))

;; toggles
(defhydra mercury-toggles (:color blue :columns 5 :hint nil)
  "
Toggle:
  _l_: Line numbers        _t_: toggle-truncate-lines
  _s_: eshell              _r_: Read-only mode"
  ("t" toggle-truncate-lines)
  ("l" display-line-numbers-mode)
  ("s" embark-eshell)
  ("r" read-only-mode)
  ("<escape>" enter-sulfur-cmd-mode "Exit" :exit t))

;;  projectile
(defhydra mercury-project (:color blue :columns 5)
  "Project"
  ("a" projectile-add-known-project "Add new project")
  ("b" projectile-switch-to-buffer "Switch to project buffer")
  ("c" project-compile "Compile in project")
  ("d" projectile-remove-known-project "Remove known project")
  ("f" projectile-find-file "Find file in project")
  ("i" projectile-invalidate-cache "Invalidate project cache")
  ("p" projectile-switch-project "Switch project")
  ("<escape>" enter-sulfur-cmd-mode "Exit"))


(provide 'mercury)
;;; mercury.el ends here
