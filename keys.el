;; keys.el --- Custom key bindings for Emacs
;;
;; Author:	Mariusz Nowak <mariusz+emacs@medikoo.com>
;; Copyright (C) 2010, 2011 Mariusz Nowak <mariusz+emacs@medikoo.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.	 See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary
;;
;; Custom key bindings for emacs. To override other bindings we load them with
;; minor-mode.

(require 'el-kit/buffer)
(require 'el-kit/file)

(defvar estarter-keys-minor-mode-map (make-keymap)
	"estarter-keys-minor-mode keymap.")

(define-key estarter-keys-minor-mode-map (kbd "C-c n")
	'el-kit-buffer-whitespace-cleanup)
(define-key estarter-keys-minor-mode-map (kbd "C-c p")
	'el-kit-buffer-print-file-name)
(define-key estarter-keys-minor-mode-map (kbd "C-w")
	'backward-kill-word)
(define-key estarter-keys-minor-mode-map (kbd "C-S-w")
	'kill-word)
(define-key estarter-keys-minor-mode-map (kbd "C-x C-k")
	'kill-region)
(define-key estarter-keys-minor-mode-map (kbd "<C-tab>")
	'el-kit-buffer-insert-tab-space)
(define-key estarter-keys-minor-mode-map (kbd "C-x f")
	'el-kit-file-recent-ido)
(define-key estarter-keys-minor-mode-map (kbd "C-c r")
	'el-kit-buffer-rename-file-or-buffer)

;; Help should search more than just commands
(define-key estarter-keys-minor-mode-map (kbd "C-h a") 'apropos)

(define-minor-mode estarter-keys-minor-mode
	"A minor mode that overrides specified key settings (in all major modes)."
	t " estarter-keys" 'estarter-keys-minor-mode-map)

(estarter-keys-minor-mode 1)

(defun estarter-keys-minor-mode-off ()
	(estarter-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'estarter-keys-minor-mode-off)

(defun estarter-keys-prority ()
	(if (not (eq (car (car minor-mode-map-alist)) 'estarter-keys-minor-mode))
		(let (gotit x)
			(progn
				(setq gotit nil)
				(dolist (x minor-mode-map-alist)
					(if (eq (car x) 'estarter-keys-minor-mode)
						(setq gotit x)))
				(if gotit
					(progn
						(delete gotit minor-mode-map-alist)
						(setq minor-mode-map-alist
							(cons gotit minor-mode-map-alist))))))))

(add-hook 'after-change-major-mode-hook 'estarter-keys-prority)
