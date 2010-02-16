;; mode-helpers.el --- Configuration functions for major mode hooks
;;
;; Copyright (C) 2008, 2009 Phil Hagelberg <technomacy@gmail.com>
;; Copyright (C) 2010 Mariusz Nowak <mariusz+emacs@medikoo.com>
;; Author: Phil Hagelberg <technomacy@gmail.com>
;; Author: Mariusz Nowak <mariusz+emacs@medikoo.com>

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
;; Configuration methods for major modes

(defvar my-coding-hook nil
	"Hook that gets run on activation of any programming mode.")

(defun my-coding-hook-run ()
	"Enable things that are convenient across all coding buffers."
	(run-hooks 'my-coding-hook))

(defun my-column-number-mode ()
	"Turn on `column-number-mode'"
	(column-number-mode t))

(defun my-hl-line-mode ()
	"Turn on `hl-line-mode'"
	(hl-line-mode t))

(defun my-whitespace-mode ()
	"Turn on `whitespace-mode'"
	(whitespace-mode -1)
	(whitespace-mode 1))

;; Reload hooks after dir locals update
(defun my-hack-local-variables-hook ()
	(run-hooks (intern (concat (symbol-name major-mode) "-hook"))))
(add-hook 'hack-local-variables-hook 'my-hack-local-variables-hook)

(defun my-paredit-mode ()
	"Turn on `paredit-mode'"
	(paredit-mode 1))

;; Copyright (C) 2008, 2009, 2010 Phil Hagelberg <technomacy@gmail.com>
;; http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-lisp.el
(defun my-pretty-lambdas ()
	"Pretty lambdas for lisp modes."
	(font-lock-add-keywords
		nil `(("(?\\(lambda\\>\\)"
				(0 (progn (compose-region (match-beginning 1) (match-end 1)
							,(make-char 'greek-iso8859-7 107))
						nil))))))

;; Copyright (C) 2008, 2009, 2010 Phil Hagelberg <technomacy@gmail.com>
;; http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-lisp.el
(defun my-remove-elc-on-save ()
	"If you're saving an elisp file, likely the .elc is no longer valid."
	(make-local-variable 'after-save-hook)
	(add-hook 'after-save-hook
		(lambda ()
			(if (file-exists-p (concat buffer-file-name "c"))
				(delete-file (concat buffer-file-name "c"))))))