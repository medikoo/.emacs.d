;; mode-helpers.el --- Custom functions used for major modes configuration
;;
;; Copyright (C) 2010, 2011 Mariusz Nowak <mariusz+emacs-starter@medikoo.com>
;; Author: Mariusz Nowak <mariusz+emacs-starter@medikoo.com>

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
;; Custom functions used for major modes configuration

(defvar estarter-coding-hook nil
	"Hook that gets run on activation of any programming mode.")

(defun estarter-coding-hook-run ()
	"Enable things that are convenient across all coding buffers."
	(run-hooks 'estarter-coding-hook))

(defun estarter-column-number-mode ()
	"Turn on `column-number-mode'"
	(column-number-mode t))

(defun estarter-hl-line-mode ()
	"Turn on `hl-line-mode'"
	(hl-line-mode t))

(defun estarter-whitespace-mode ()
	"Turn on `whitespace-mode'"
	(whitespace-mode -1)
	(whitespace-mode 1))

(defun estarter-reset-indent-tabs-mode ()
	"Reset `indent-tabs-mode' to its default.
	Some modes force own setting, we may don't like that."
	(kill-local-variable 'indent-tabs-mode))

(defun estarter-reset-tab-width ()
	"Reset `tab-width' to its default.
	Some modes force own setting, we may don't like that."
	(kill-local-variable 'tab-width))

;; Make sure dir locals are set after major mode change.
;; Load (my) major mode hooks afterwards.
(defun estarter-after-change-major-mode ()
	(hack-local-variables)
	(run-hooks (intern (concat "estarter-" (symbol-name major-mode) "-hook"))))
(add-hook 'after-change-major-mode-hook 'estarter-after-change-major-mode)

(defun estarter-paredit-mode ()
	"Turn on `paredit-mode'"
	(paredit-mode 1))

;; Copyright (C) 2008, 2009, 2010 Phil Hagelberg <technomacy@gmail.com>
;; http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-lisp.el
(defun estarter-pretty-lambdas ()
	"Pretty lambdas for lisp modes."
	(font-lock-add-keywords
		nil `(("(?\\(lambda\\>\\)"
				(0 (progn (compose-region (match-beginning 1) (match-end 1)
							,(make-char 'greek-iso8859-7 107))
						nil))))))

;; Copyright (C) 2008, 2009, 2010 Phil Hagelberg <technomacy@gmail.com>
;; http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-lisp.el
(defun estarter-remove-elc-on-save ()
	"If you're saving an elisp file, likely the .elc is no longer valid."
	(make-local-variable 'after-save-hook)
	(add-hook 'after-save-hook
		(lambda ()
			(if (file-exists-p (concat buffer-file-name "c"))
				(delete-file (concat buffer-file-name "c"))))))

(defun estarter-ftl-support ()
	(interactive)
	(if buffer-file-name
		(if (string-equal (substring	buffer-file-name -4) ".ftl")
			(ftl-mode 1))))

(defun estarter-yas/expand ()
	"Prevent yasnippet expansion after other command than `self-insert-command'"
	(interactive)
	(if (eq last-command 'self-insert-command)
		(yas/expand)

		;;; Copied from `yas/expand-1'
		(cond ((eq yas/fallback-behavior 'return-nil)
				;; return nil
				nil)
			((eq yas/fallback-behavior 'call-other-command)
				(let* ((yas/minor-mode nil)
						(keys-1 (this-command-keys-vector))
						(keys-2 (and yas/trigger-key
								(stringp yas/trigger-key)
								(read-kbd-macro yas/trigger-key)))
						(command-1 (and keys-1 (key-binding keys-1)))
						(command-2 (and keys-2 (key-binding keys-2)))
						(command (or (and (not (eq command-1 'yas/expand))
									command-1)
								command-2)))
					(when (and (commandp command)
							(not (eq 'yas/expand command)))
						(setq this-command command)
						(call-interactively command))))
			((and (listp yas/fallback-behavior)
					(cdr yas/fallback-behavior)
					(eq 'apply (car yas/fallback-behavior)))
				(if (cddr yas/fallback-behavior)
					(apply (cadr yas/fallback-behavior)
						(cddr yas/fallback-behavior))
					(when (commandp (cadr yas/fallback-behavior))
						(setq this-command (cadr yas/fallback-behavior))
						(call-interactively (cadr yas/fallback-behavior)))))
			(t
				;; also return nil if all the other fallbacks have failed
				nil))))
