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

;; clojure-mode
(add-hook 'clojure-mode-hook 'my-coding-hook-run)
(add-hook 'clojure-mode-hook 'my-paredit-mode)
(add-hook 'clojure-mode-hook 'my-pretty-lambdas)
(require 'my-indent/lisp)
(add-hook 'clojure-mode-hook 'my-indent-set-lisp)

;; conf-mode
(add-hook 'conf-mode-hook 'my-coding-hook-run)
(require 'my-indent/conf)
(add-hook 'conf-mode-hook 'my-indent-set-conf)

;; css-mode
(add-hook 'css-mode-hook 'my-coding-hook-run)

;; dns-mode
(add-hook 'dns-mode-hook 'my-coding-hook-run)
(require 'my-indent/dns)
(add-hook 'dns-mode-hook 'my-indent-set-dns)

;; emacs-lisp mode
(add-hook 'emacs-lisp-mode-hook 'my-coding-hook-run)
(add-hook 'emacs-lisp-mode-hook 'my-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'my-pretty-lambdas)
(require 'my-indent/lisp)
(add-hook 'emacs-lisp-mode-hook 'my-indent-set-lisp)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'my-remove-elc-on-save)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; haml-mode
(add-hook 'haml-mode-hook 'my-coding-hook-run)

;; lisp-mode
(add-hook 'lisp-mode-hook 'my-coding-hook-run)
(add-hook 'lisp-mode-hook 'my-paredit-mode)
(add-hook 'lisp-mode-hook 'my-pretty-lambdas)
(require 'my-indent/lisp)
(add-hook 'lisp-mode-hook 'my-indent-set-lisp)

;; php-mode
(add-hook 'php-mode-hook 'my-coding-hook-run)

;; python-mode
(add-hook 'python-mode-hook 'my-coding-hook-run)

;; ruby-mode
(add-hook 'ruby-mode-hook 'my-coding-hook-run)

;; sass-mode
(add-hook 'sass-mode-hook 'my-coding-hook-run)

;; scheme-mode
(add-hook 'scheme-mode-hook 'my-coding-hook-run)
(add-hook 'scheme-mode-hook 'my-paredit-mode)
(add-hook 'scheme-mode-hook 'my-pretty-lambdas)
(require 'my-indent/lisp)
(add-hook 'scheme-mode-hook 'my-indent-set-lisp)