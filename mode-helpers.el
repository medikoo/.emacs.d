;; mode-helpers.el --- Custom functions used for major modes configuration  -*- lexical-binding: t -*-
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

(require 'el-kit/buffer)
(require 'show-point-mode)
(require 'yasnippet)
(declare-function paredit-mode "paredit")
(declare-function ftl-mode "ftl")
(defvar estarter-tab-width-name)

(defvar estarter-coding-hook nil
	"Hook that gets run on activation of any programming mode.")

(defun estarter-coding-hook-run ()
	"Enable things that are convenient across all coding buffers."
	(run-hooks 'estarter-coding-hook))

(defun estarter-column-number-mode ()
	"Turn on `column-number-mode'."
	(column-number-mode t))

(defun estarter-hl-line-mode ()
	"Turn on `hl-line-mode'."
	(hl-line-mode t))

(defun estarter-whitespace-mode ()
	"Turn on `whitespace-mode'."
	(whitespace-mode -1)
	(whitespace-mode 1))

(defun estarter-show-point-mode ()
	"Turn on `show-point-mode'."
	(show-point-mode t))

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

(defun estarter-reset-indent-line ()
	(setq-local indent-line-function nil))

(defun estarter-indent-whitespace ()
	(if indent-tabs-mode
		"	"
		(let ((w ""))
			(dotimes (_ (symbol-value estarter-tab-width-name))
				(setq w (concat w " ")))
			w)))

(defvar estarter-yas/expand-prevent nil
	"Whether to prevent snippet expansion.")

(defun estarter-yas/expand--only-on-self-insert (orig-fun &rest args)
	"Prevent snippet expansion on other command than `self-insert-command'."
	(unless (eq last-command 'self-insert-command)
		(setq estarter-yas/expand-prevent t))
	(unwind-protect
		(apply orig-fun args)
		(setq estarter-yas/expand-prevent nil)))
(advice-add 'yas/expand :around #'estarter-yas/expand--only-on-self-insert)

(defun estarter-yas/get-snippet-tables--only-on-self-insert (orig-fun &rest args)
	"Do not return snippets table if prevent expansion is on."
	(unless estarter-yas/expand-prevent
		(apply orig-fun args)))
(advice-add 'yas/get-snippet-tables :around
	#'estarter-yas/get-snippet-tables--only-on-self-insert)

(defun estarter-indent-sexp--use-indent-line-function (&optional endpos)
	"Force `indent-sexp' to use `indent-line-function'."
	(let ((startpos (point)))
		(save-excursion
			(unless endpos
				(forward-list))
			(indent-region startpos (or endpos (point))))))
(advice-add 'indent-sexp :override #'estarter-indent-sexp--use-indent-line-function)

(defvar js2-buffer-file-name nil
	"`js2-mode' methods sets `buffer-file-name' to nil for internal processing.
	However we need that value to detect whether we're in JSON file.
	Before it is cleared we save `buffer-file-name' under this name.")

;; Variables of the (dynamically bound) vendored js2.el that advices below
;; rely on. Declared so they're accessed dynamically under lexical binding.
(with-suppressed-warnings ((lexical tt)) (defvar tt))
(defvar js2-LC)
(defvar js2-ts-cursor)
(defvar js2-basic-offset)
(declare-function js2-parse-assign-expr "js2")
(declare-function js-ctrl-statement-indentation "js2")
(declare-function js-continued-expression-p "js2")
(declare-function js2-same-line "js2")
(declare-function js2-indent-in-array-comp "js2")
(declare-function js2-array-comp-indentation "js2")

(defun estarter-js2-reparse--json (&rest _)
	"Update `js2-buffer-file-name'."
	(setq js2-buffer-file-name buffer-file-name))
(advice-add 'js2-reparse :before #'estarter-js2-reparse--json)

(defun estarter-js2-parse-statement--json (orig-fun &rest args)
	"Parse JSON file differently.
	Use `js2-parse-assign-expr' if within and at begin of JSON file."
	(if (and (= tt js2-LC)
			js2-buffer-file-name
			(string-equal (substring js2-buffer-file-name -5) ".json")
			(eq (+ (el-kit-buffer-first-nonwhitespace-pos) 1) js2-ts-cursor))
		(js2-parse-assign-expr)
		(apply orig-fun args)))
(advice-add 'js2-parse-statement :around #'estarter-js2-parse-statement--json)

(defun estarter-js2-mode--run-estarter-mode-hooks (&rest _)
	"`js2-mode' is not build with `define-derived-mode',
	in consequence it does not run `after-change-major-mode-hook'.
	This advice makes sure it gets run."
	(estarter-after-change-major-mode))
(advice-add 'js2-mode :after #'estarter-js2-mode--run-estarter-mode-hooks)

(defun estarter-js-proper-indentation (parse-status)
	"Return the proper indentation for the current line.
	Replaces `js-proper-indentation' (js2) and `js--proper-indentation' (js)."
	(save-excursion
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
								(line-beginning-position)))) ; at or after first loop?
					(js2-array-comp-indentation parse-status beg))
				(ctrl-stmt-indent)

				(bracket
					(goto-char bracket)
					(cond
						((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
							(parse-partial-sexp (line-beginning-position) (point))
							(when (save-excursion (skip-chars-backward " \t)")
									(looking-at ")"))
								(backward-list))
							(back-to-indentation)
							(cond (same-indent-p
									(current-column))
								(continued-expr-p
									(+ (current-column) (* 2 js2-basic-offset)))
								(t
									(+ (current-column) js2-basic-offset))))
						(t
							(back-to-indentation)
							(unless same-indent-p
								(forward-char js2-basic-offset)
								(skip-chars-forward " \t"))
							(current-column))))

				(continued-expr-p js2-basic-offset)
				(t 0)))))
(advice-add 'js-proper-indentation :override #'estarter-js-proper-indentation)
(advice-add 'js--proper-indentation :override #'estarter-js-proper-indentation)

(defun estarter-js-indent-line--vars (&rest _)
	"Correct indentation for coma separated var values."
	(save-excursion
		(back-to-indentation)
		(if (looking-at ",")
			(insert "  "))))
(advice-add 'js2-indent-line :after #'estarter-js-indent-line--vars)
(advice-add 'js-indent-line :after #'estarter-js-indent-line--vars)

	(defun estarter-js2-tab-width-name ()
		"Set js2-mode tab-width variable name"
		(setq estarter-tab-width-name 'js2-basic-offset))

	(defun estarter-js2-packagejson ()
		"Different whitespace settings"
		(if (string-equal (substring js2-buffer-file-name -13) "/package.json")
			(setq indent-tabs-mode nil)))

	(require 'vc-dir)
	(defvar use-vc-backend) ; dynamically bound, read by `vc-dir-mode'
	(defun vc-dir-revert-buffer-function (&optional _ignore-auto _noconfirm)
		"Real refresh for vc-dir buffers (works as reload)."
		(goto-char 0)
		(let*
			((index (search-forward "Working dir: " nil t))
				(path (if index  (buffer-substring-no-properties index (- (search-forward "\n" nil t) 1))))
				(window (selected-window))
				backend buffer)
			(when path
				(kill-buffer (current-buffer))
				(setq backend (vc-responsible-backend path))
				(setq buffer (get-buffer (vc-dir-prepare-status-buffer "*vc-dir*" path backend)))
				(set-buffer buffer)
				(set-window-buffer window buffer)
				(if (derived-mode-p 'vc-dir-mode)
					(vc-dir-refresh)
				;; FIXME: find a better way to pass the backend to `vc-dir-mode'.
				(let ((use-vc-backend backend))
					(vc-dir-mode)))
)))
