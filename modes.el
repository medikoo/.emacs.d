;; modes.el --- Custom configuration for major modes
;;
;; Author:	Mariusz Nowak <mariusz+emacs@medikoo.com>
;; Copyright (C) 2010 Mariusz Nowak <mariusz+emacs@medikoo.com>

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
;; Custom configuration for major modes. Some of the referenced functions can
;; be found in mode-helpers.el

;; all
(add-hook 'my-coding-hook 'my-column-number-mode)
(add-hook 'my-coding-hook 'my-hl-line-mode)
(add-hook 'my-coding-hook 'my-whitespace-mode)

;; Emacs lisp source specific customizations
(dir-locals-set-class-variables 'elisp
	'((emacs-lisp-mode . ((indent-tabs-mode . nil)))))
(dir-locals-set-directory-class
	my-elisp-dir 'elisp)

;; clojure-mode
(defvar my-clojure-mode-hook nil
	"Hook that gest run on activation of `closure-mode' but after file locals.")
(add-hook 'my-clojure-mode-hook 'my-coding-hook-run)
(add-hook 'my-clojure-mode-hook 'my-paredit-mode)
(add-hook 'my-clojure-mode-hook 'my-pretty-lambdas)
(require 'my-indent/lisp)
(add-hook 'my-clojure-mode-hook 'my-indent-set-lisp)

;; conf-mode
(add-to-list 'auto-mode-alist '("/sites-\\(available\\|enabled\\)/" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))
(defvar my-conf-mode-hook nil
	"Hook that gest run on activation of `conf-mode' but after file locals.")
(add-hook 'my-conf-mode-hook 'my-coding-hook-run)
(require 'my-indent/conf)
(add-hook 'my-conf-mode-hook 'my-indent-set-conf)

;; css-mode
(defvar my-css-mode-hook nil
	"Hook that gest run on activation of `css-mode' but after file locals.")
(add-hook 'my-css-mode-hook 'my-coding-hook-run)

;; django-mode
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . django-mode))

;; dns-mode
(add-to-list 'magic-mode-alist '("\\$TTL[ \t]+[0-9]+[ \t]*\n" . dns-mode))
(defvar my-dns-mode-hook nil
	"Hook that gest run on activation of `dns-mode' but after file locals.")
(add-hook 'my-dns-mode-hook 'my-coding-hook-run)
(require 'my-indent/dns)
(add-hook 'my-dns-mode-hook 'my-indent-set-dns)

;; emacs-lisp mode
(defvar my-emacs-lisp-mode-hook nil
	"Hook that gest run on activation of `emacs-lisp-mode' but after file locals.")
(add-hook 'my-emacs-lisp-mode-hook 'my-coding-hook-run)
(add-hook 'my-emacs-lisp-mode-hook 'my-paredit-mode)
(add-hook 'my-emacs-lisp-mode-hook 'my-pretty-lambdas)
(require 'my-indent/lisp)
(add-hook 'my-emacs-lisp-mode-hook 'my-indent-set-lisp)
(add-hook 'my-emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'my-emacs-lisp-mode-hook 'my-remove-elc-on-save)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; haml-mode
(defvar my-haml-mode-hook nil
	"Hook that gest run on activation of `haml-mode' but after file locals.")
(add-hook 'my-haml-mode-hook 'my-coding-hook-run)
(add-hook 'haml-mode-hook 'my-reset-indent-tabs-mode)

;; html-mode
(defvar my-html-mode-hook nil
	"Hook that gest run on activation of `html-mode' but after file locals.")
(add-to-list 'auto-mode-alist '("\\.ftl$" . html-mode))
(add-hook 'my-html-mode-hook 'my-ftl-support)
(add-hook 'my-html-mode-hook 'my-coding-hook-run)

;; js2-mode
(defvar my-js2-mode-hook nil
	"Hook that gest run on activation of `js2-mode' but after file locals.")
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-hook 'my-js2-mode-hook 'my-coding-hook-run)

(eval-after-load 'js2-mode
	'(progn
		;; Fix weird js2-mode indent rules
		(defun js-proper-indentation (parse-status)
			"Return the proper indentation for the current line."
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
						(t 0)))))

		(defun my-js2-mode-split-string (parse-status)
			"Turn a newline in mid-string into a string concatenation."
			(let* ((col (current-column))
					(quote-char (nth 3 parse-status))
					(quote-string (string quote-char))
					(string-beg (nth 8 parse-status))
					(indent (save-match-data
							(or
								(save-excursion
									(back-to-indentation)
									(if (looking-at "\\+")
										(current-column)))
								(save-excursion
									(goto-char string-beg)
									(if (looking-back "\\+\\s-+")
										(goto-char (match-beginning 0)))
									(current-column))))))
				(insert quote-char "\n")
				(indent-to indent)
				(insert "+ " quote-string)
				(when (eolp)
					(insert quote-string)
					(backward-char 1))))


))

;; lisp-mode
(defvar my-lisp-mode-hook nil
	"Hook that gest run on activation of `lisp-mode' but after file locals.")
(add-hook 'my-lisp-mode-hook 'my-coding-hook-run)
(add-hook 'my-lisp-mode-hook 'my-paredit-mode)
(add-hook 'my-lisp-mode-hook 'my-pretty-lambdas)
(require 'my-indent/lisp)
(add-hook 'my-lisp-mode-hook 'my-indent-set-lisp)

;; php-mode
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(defvar my-php-mode-hook nil
	"Hook that gest run on activation of `php-mode' but after file locals.")
(add-hook 'my-php-mode-hook 'my-coding-hook-run)

;; python-mode
(defvar my-python-mode-hook nil
	"Hook that gest run on activation of `python-mode' but after file locals.")
(add-hook 'my-python-mode-hook 'my-coding-hook-run)
(add-hook 'python-mode-hook 'my-reset-indent-tabs-mode)
(add-hook 'python-mode-hook 'my-reset-tab-width)

;; ruby-mode
(defvar my-ruby-mode-hook nil
	"Hook that gest run on activation of `ruby-mode' but after file locals.")
(add-hook 'my-ruby-mode-hook 'my-coding-hook-run)

;; sass-mode
(defvar my-sass-mode-hook nil
	"Hook that gest run on activation of `sass-mode' but after file locals.")
(add-hook 'my-sass-mode-hook 'my-coding-hook-run)

;; scheme-mode
(defvar my-scheme-mode-hook nil
	"Hook that gest run on activation of `scheme-mode' but after file locals.")
(add-hook 'my-scheme-mode-hook 'my-coding-hook-run)
(add-hook 'my-scheme-mode-hook 'my-paredit-mode)
(add-hook 'my-scheme-mode-hook 'my-pretty-lambdas)
(require 'my-indent/lisp)
(add-hook 'my-scheme-mode-hook 'my-indent-set-lisp)