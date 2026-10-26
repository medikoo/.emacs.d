;; init.el --- Where everything begins  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2010, 2011 Mariusz Nowak <mariusz+emacs-starter@medikoo.com>
;; Author: Mariusz Nowak <mariusz+emacs-starter@medikoo.com>
;; Inpired and initially based on Phil Hagelberg's Emacs Starter kit
;; https://github.com/technomancy/emacs-starter-kit

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
;; See README.

;; Configure interface

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set coding system
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set environment variables
(defconst estarter-dotfiles-dir (file-name-directory (or (buffer-file-name)
			load-file-name))
	"Path to your .emacs.d folder.")

(defconst estarter-elisp-dir (let ((path (symbol-file 'add-hook 'defun)))
		(substring path 0 (+ 1 (string-match "/lisp/" path))))
	"Path to Emacs lisp files folder.")

(defconst estarter-vendor-dir (concat estarter-dotfiles-dir "vendor/")
	"Path to external modules folder.")

(setq custom-file (concat estarter-dotfiles-dir "custom.el"))
(add-to-list 'load-path estarter-vendor-dir)

;; Compile when needed
(defun estarter-recompile-all ()
	(let ((noninteractive t))
		(byte-recompile-directory estarter-dotfiles-dir 0)))
(estarter-recompile-all)
(add-hook 'kill-emacs-hook 'estarter-recompile-all)

;; Native compilation (Emacs 28+) compiles the vendored (pre `cl-lib') libraries
;; asynchronously and reports every "function X is not known to be defined"
;; into *Warnings*. Keep those reports in *Native-compile-Log* only.
(defvar native-comp-async-report-warnings-errors)
(setq native-comp-async-report-warnings-errors 'silent)

;; `cl' is obsolete, but vendored modules (el-kit, el-screen, el-indent,
;; el-fast-filelist) still rely on its non prefixed `second', `third',
;; `copy-list', `lexical-let*' etc.
(with-suppressed-warnings ((obsolete cl)) (require 'cl))

;; Generate and load autoloads
(defconst estarter-autoload-file (concat estarter-dotfiles-dir "loaddefs.el")
	"Path to generated autoloads file.")
(loaddefs-generate estarter-vendor-dir estarter-autoload-file)
(load estarter-autoload-file)

;; Load needed modules (ones that won't go automatically through autoload)
(autoload 'paredit-mode "paredit"
	"Minor mode for pseudo-structurally editing Lisp code." t)
(require 'show-point-mode)
(require 'saveplace)
(require 'el-screen/el-screen)
(require 'markdown-mode)
(require 'sml-modeline)
(require 'color-theme)
;; By default color-theme loads every file in vendor/themes (both .el and .elc,
;; including the huge color-theme-library). Load just our theme, byte-compiled.
(setq color-theme-libraries
	(list (concat estarter-vendor-dir "themes/color-theme-estarter")))

;; Turn on helpful modes
(blink-cursor-mode 1)
(auto-compression-mode t)
(recentf-mode 1)
(show-paren-mode 1)
(sml-modeline-mode)
(color-theme-initialize)
(require 'yasnippet)
(yas/initialize)

;; Fixes for broken or not working properly functions
(load (concat estarter-dotfiles-dir "fix"))

;; Seed the random-number generator
(random t)

;; Make life easier
(setq use-short-answers t)

;; Load extra functions
(load (concat estarter-dotfiles-dir "defun"))

;; Customize key bindings
(load (concat estarter-dotfiles-dir "keys"))

;; Major Mode-specific customizations
(load (concat estarter-dotfiles-dir "mode-helpers"))
(load (concat estarter-dotfiles-dir "modes"))

;; System-specific customizations
(load (concat estarter-dotfiles-dir (symbol-name system-type)) 'noerror)

;; Defaults
(defgroup estarter nil "estarter -- initializaton setup"
	:group 'convenience)

(defcustom estarter-tab-width 2
	"Default tab width."
	:tag "Default tab width"
	:type 'integer
	:group 'estarter)
(defvar estarter-tab-width-name 'estarter-tab-width
	"Name of variable that holds tab width for current major mode.")
(make-variable-buffer-local 'estarter-tab-width-name)

(defcustom estarter-indent-tabs-mode t
	"Indent with tabs ?"
	:tag "Indent with tabs ?"
	:type 'boolean
	:group 'estarter)

(defcustom estarter-color-theme 'color-theme-estarter-charcoal-black
	"Default color-theme."
	:tag "Default color-theme"
	:type '(string :size 64)
	:group 'estarter)

(defcustom estarter-frame-alpha 93
	"Default frame-alpha setting."
	:tag "Default frame-alpja setting"
	:type 'integer
	:group 'estarter)

;; User-specific customizations

;; Remove annoying keybindings
;; Hides/minimizes Emacs
(global-unset-key (kbd "C-x C-z"))
;; Displays warning confirm box for downcase-region command
(global-unset-key (kbd "C-x C-l"))

;; Load Snippets
(yas/load-directory (concat estarter-vendor-dir "snippets"))

;; Load user config file (if it exists)
(load (concat estarter-dotfiles-dir "config.el") 'noerror)

;; Load user custom settings
(load custom-file 'noerror)

;; Load color theme
(funcall estarter-color-theme)
;; Emacs's win.el will override some face values
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434
;; so we're loading it later again through `window-setup-hook'
(add-hook 'window-setup-hook estarter-color-theme)

;; Set frame alpha
(modify-all-frames-parameters (list (cons 'alpha estarter-frame-alpha)))

;; Load screen manager
(el-screen-init)
(put 'dired-find-alternate-file 'disabled nil)
