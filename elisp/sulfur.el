;;; sulfur.el --- Vi-like emulator -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 cervelleless
;;
;; Author: cervelleless <http://github.com/cervelleless>
;; Maintainer: cervelleless <cervelleless@gmail.com>
;; Created: November 27, 2020
;; Modified: November 27, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/felix/sulfur
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Vi-like emulator.  It's rudimentary,.
;;
;;; Code:

;; (require 'bind-key)
(use-package crux
  :straight t
  :defer t)

(defvar sulfur-cmd-map (make-sparse-keymap)
  "Keymap for sulfur-cmd-mode.")

;;;###autoload
(define-minor-mode sulfur-cmd-mode
  "A Vi-like command mode."
  :init-value t
  :lighter " Sulfur-cmd"
  :keymap sulfur-cmd-map
  (setq cursor-type 'box)
  (add-to-ordered-list 'emulation-mode-map-alists `((sulfur-cmd-mode . ,sulfur-cmd-map)) 0))

;;;###autoload
(define-globalized-minor-mode global-sulfur-cmd-mode sulfur-cmd-mode sulfur-cmd-mode)

;;;###autoload
(defun enter-sulfur-cmd-mode ()
  "Enter sulfur-cmd-mode."
  (interactive)
  "Enter sulfur-cmd-mode."
  (sulfur-cmd-mode 1))

;;;###autoload
(defun quit-sulfur-cmd-mode ()
  "Quit sulfur-cmd-mode."
  (interactive)
  "Quit sulfur-cmd-mode."
  (sulfur-cmd-mode -1)
  (setq cursor-type 'bar))

(add-hook 'minibuffer-setup-hook 'quit-sulfur-cmd-mode)


(defun sulfur/char-existp (chr)
  "Check the input CHR exist in the sulfur-string table or not."
  (let ((str (split-string "`1234567890-=~!@#$%^&*()_+qwertyuiop[]\\QWERTYUIOP{}|asdfghjkl ;'ASDFGHJKL:\"zxcvbnm,./ZXCVBNM<>?" "")))
  (seq-contains-p str (char-to-string chr))))

;; commands
;;;###autoload
(defun sulfur/enter-editting (&optional arg)
  "Quit sulfur-cmd-mode for edit.  If the ARG is not nil, leave the message -- INSERT --."
  (interactive)
  (quit-sulfur-cmd-mode)
  (unless (boundp 'arg)
    (message "-- INSERT --")))

;;;###autoload
(defun sulfur/replace-char ()
  "Replace char."
  (interactive)
  (setq cursor-type '(hbar . 5))
  (let ((key (read-char)))
    (if (sulfur/char-existp key)
        (progn (insert key)
               (delete-char 1)
               (backward-char)
               (setq cursor-type 'box))
      (progn (message "Quit")
             (setq cursor-type 'box)))))


;;;###autoload
(defun sulfur/line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun sulfur/line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;;###autoload
(defun sulfur/copy-or-yank ()
  "Copy or paste."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (yank)))
  
;;;###autoload
(defun sulfur/delete-relatives ()
  "Erase text relative commands."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)
    (let ((key (read-char)))
      (if (sulfur/char-existp key)
	  (cond ((char-equal key ?b) (backward-kill-word))
		((char-equal key ?d) (crux-kill-whole-line))
		((char-equal key ?w) (progn (forward-word)
					    (backward-word)
					    (kill-word 1)))
		((char-equal key ?0) (kill-line 0))
		((char-equal key ?$) (kill-line 1))
		(t (message "d %s is undefined" (char-to-string key))))
	(message "Quit")))))

;;;###autoload
(defun sulfur/colon-cmd ()
  "Ex-like command."
  (interactive)
  (let ((key (read-string ":")))
    (cond ((string-equal key "w") (call-interactively 'save-buffer))
          ((string-equal key "wq") (progn (call-interactively 'save-buffer)
                                         (quit-window)
                                         (switch-to-buffer "*astrolabe*")))
	  ((string-equal key "q!") (progn (quit-window)
					 (switch-to-buffer "*astrolabe*")))
          (t (message "%s is undefined" key)))))

;;;###autoload
(defun sulfur/quit-or-kmacro ()
  "Quit window or launch keyboard macro system."
  (interactive)
  (if (eq (get major-mode 'mode-class) 'special)
      (quit-window)
    (let ((key (read-char)))
      (if (sulfur/char-existp key)
	  (cond ((char-equal key ?q) (call-interactively 'kmacro-start-macro))
		((char-equal key ?w) (call-interactively 'kmacro-end-macro))
		((char-equal key ?e) (call-interactively 'kmacro-end-and-call-macro)))))))

;; keybinding
(bind-keys :map sulfur-cmd-map
           ("a" . (lambda () (interactive)
		    (progn (unless (eolp)
			     (forward-char))
			   (sulfur/enter-editting))))
	   ("b" . backward-word)
	   ("cb" . (lambda () (interactive)
		     (progn (backward-kill-word 1)
			    (quit-sulfur-cmd-mode))))
	   ("cf" . (lambda () (interactive)
		     (kill-word 1)
		     (quit-sulfur-cmd-mode)))
	   ("cw" . (lambda () (interactive)
		     (progn (forward-word)
			    (backward-word)
			    (kill-word 1)
			    (quit-sulfur-cmd-mode))))
	   ("c$" . (lambda () (interactive)
		     (progn (crux-smart-kill-line)
			    (quit-sulfur-cmd-mode))))
	   ("d" . sulfur/delete-relatives)
	   ;; ("db" . backward-kill-word)
	   ;; ("dd" . crux-kill-whole-line)
	   ;; ("dw" . (lambda () (interactive)
	   ;; 	     (progn (forward-word)
	   ;; 		    (backward-word)
	   ;; 		    (kill-word 1))))
	   ;; ("d$" . (kill-line 1))
	   ;; ("d0" . (kill-line 0))
	   ("e" . nil)
	   ("f" . nil)
	   ("gg" . beginning-of-buffer)
	   ("h" . (lambda () (interactive)
		    (if (bolp)
			(message "Beginning of line")
		      (backward-char))))
	   ("i" . sulfur/enter-editting)
	   ("j" . next-line)
	   ("k" . previous-line)
	   ("l" . (lambda () (interactive)
		    (forward-char)
		    (if (eolp)
			(progn (message "End of line")
			       (backward-char 1)))))
	   ("m" . nil)
	   ("n" . isearch-repeat-forward)
	   ("o" . (lambda () (interactive)
		    (crux-smart-open-line nil)
		    (quit-sulfur-cmd-mode)))
	   ("p" . (lambda () (interactive)
		    (end-of-line)
		    (newline-and-indent)
		    (yank)
		    (delete-char -1)
		    (beginning-of-line)))
	   ("q" . sulfur/quit-or-kmacro)
	   ("r" . sulfur/replace-char)
	   ("sb" . avy-goto-word-1-above)
	   ("sf" . avy-goto-word-1-below)
	   ("sw" . consult-line)
	   ("sr" . anzu-query-replace)
	   ("ss" . avy-goto-char-2)
	   ("t" . nil)
	   ("uu" . undo-tree-undo)
	   ("uv" . undo-tree-visualize)
	   ("v" . set-mark-command)
	   ("w" . forward-to-word)
	   ("x" . delete-char)
	   ("y" . sulfur/copy-or-yank)
	   ("z" . nil)
	   ("A" . (lambda () (interactive)
		    (move-end-of-line nil)
		    (quit-sulfur-cmd-mode)
		    (message "-- INSERT --")))
	   ("B" . nil)
	   ("C" . capitalize-word)
	   ("D" . crux-smart-kill-line)
	   ("E" . nil)
	   ("F" . nil)
	   ("G" . end-of-buffer)
	   ("H" . nil)
	   ("I" . (lambda () (interactive)
		    (crux-move-beginning-of-line nil)
		    (quit-sulfur-cmd-mode)))
	   ("J" . crux-top-join-line)
	   ("K" . nil)
	   ("L" . recenter)
	   ("M" . nil)
	   ("N" . isearch-repeat-backward)
	   ("O" . (lambda () (interactive)
		    (crux-smart-open-line-above)
		    (quit-sulfur-cmd-mode)))
	   ("P" . (lambda () (interactive)
		    (beginning-of-line)
		    (yank)))
	   ("Q" . nil)
	   ("R" . nil)
	   ("S" . nil)
	   ("T" . nil)
	   ("U" . upcase-word)
	   ("V" . nil)
	   ("W" . nil)
	   ("X" . nil)
	   ("Y" . nil)
	   ("Z" . nil)
           ("0" . beginning-of-line)
	   ("!" . nil)
	   ("@" . nil)
	   ("#" . nil)
	   ("$" . nil)
	   ("^" . nil)
	   ("&" . nil)
	   ("(" . nil)
	   (")" . nil)
	   ("-" . nil)
	   ("+" . nil)
	   ("_" . nil)
	   ("=" . nil)
	   ("/" . isearch-forward)
	   (":" . sulfur/colon-cmd)
	   ("-" . er/expand-region)
	   ("^" . (crux-move-beginning-of-line nil))
	   ("$" . (lambda () (interactive)
		    (move-end-of-line nil)
		    (backward-char)))
	   ;; ("," . )
	   ("M-x" . execute-extended-command)
	   ("<M-up>" . sulfur/line-up)
	   ("<M-down>" . sulfur/line-down)
	   ;; ;; ("<right>" sulfur/lambda-l)
	   ("<escape>" . keyboard-escape-quit)
	   ("SPC" . mercury/body))

(define-key global-map [escape] (lambda ()
                                  (interactive)
                                  (if (minibufferp)
                                      (keyboard-escape-quit)
                                    (unless (bolp)
                                      (backward-char))
                                    (enter-sulfur-cmd-mode))))

(define-key special-mode-map [remap scroll-up-command] 'mercury/body)

(provide 'sulfur)
;;; sulfur.el ends here
