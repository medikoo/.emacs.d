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
;; Load estarter major mode hooks afterwards.
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
	"`ftl-mode' for *.ftl files."
	(interactive)
	(if buffer-file-name
		(if (string-equal (substring	buffer-file-name -4) ".ftl")
			(ftl-mode 1))))

(defvar estarter-yas/expand-prevent nil
	"Whether to prevent snippet expansion.")

(defadvice yas/expand (around only-on-self-insert)
	"Prevent snippet expansion on other command than `self-insert-command'."
	(unless (eq last-command 'self-insert-command)
		(setq estarter-yas/expand-prevent t))
	ad-do-it
	(setq estarter-yas/expand-prevent nil))

(defadvice yas/get-snippet-tables (around only-on-self-insert)
	"Do not return snippets table if prevent expansion is on."
	(unless estarter-yas/expand-prevent
		ad-do-it))

(defadvice indent-sexp (around use-indent-line-function
		(&optional endpos))
	"Force `indent-sexp' to use `indent-line-function'."
	(let ((startpos (point)))
		(save-excursion
			(unless endpos
				(forward-list))
			(indent-region startpos (or endpos (point))))))

(defvar js2-buffer-file-name nil
	"`js2-mode' methods sets `buffer-file-name' to nil for internal processing.
	However we need that value to detect whether we're in JSON file.
	Before it is cleared we save `buffer-file-name' under this name.")

(defadvice js2-reparse (before json)
	"Update `js2-buffer-file-name'."
	(setq js2-buffer-file-name buffer-file-name))

(defadvice js2-parse-statement (around json)
	"Parse JSON file differently.
	Use `js2-parse-assign-expr' if within and at begin of JSON file."
	(if (and (= tt js2-LC)
			js2-buffer-file-name
			(string-equal (substring js2-buffer-file-name -5) ".json")
			(eq (+ (el-kit-buffer-first-nonwhitespace-pos) 1) js2-ts-cursor))
		(setq ad-return-value (js2-parse-assign-expr))
		ad-do-it))

(defadvice js2-mode (after run-estarter-mode-hooks)
	"`js2-mode' is not build with `define-derived-mode',
	in consequence it does not run `after-change-major-mode-hook'.
	This advice makes sure it gets run."
	(estarter-after-change-major-mode))

(defadvice js-proper-indentation (around fix-indent
		(parse-status))
	"Return the proper indentation for the current line."
	(setq ad-return-value (save-excursion
			(back-to-indentation)
			(let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
					(same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
					(continued-expr-p (js-continued-expression-p))
					(bracket (nth 1 parse-status))
					beg)
				(cond
					;; indent array comprehension continuation lines specially
					((and bracket
							(not (js2-same-line bracket))
							(setq beg (js2-indent-in-array-comp parse-status))
							(>= (point) (save-excursion
									(goto-char beg)
									(point-at-bol)))) ; at or after first loop?
						(js2-array-comp-indentation parse-status beg))
					(ctrl-stmt-indent)

					(bracket
						(goto-char bracket)
						(cond
							((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
								(let ((p (parse-partial-sexp (point-at-bol) (point))))
									(when (save-excursion (skip-chars-backward " \t)")
											(looking-at ")"))
										(backward-list))
									(back-to-indentation)
									(cond (same-indent-p
											(current-column))
										(continued-expr-p
											(+ (current-column) (* 2 js2-basic-offset)))
										(t
											(+ (current-column) js2-basic-offset)))))
							(t
								(back-to-indentation)
								(unless same-indent-p
									(forward-char js2-basic-offset)
									(skip-chars-forward " \t"))
								(current-column))))

					(continued-expr-p js2-basic-offset)
					(t 0))))))
