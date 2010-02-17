;; init.el --- Where everything begins
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
;; This setup is inspired by and have parts copied from Emacs Starter Kit:
;; http://github.com/technomancy/emacs-starter-kit
;;
;; This is the first thing to get loaded.

;; Interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 1))

;; Coding system
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Environment variables
(setq my-dotfiles-dir (file-name-directory (or (buffer-file-name)
			load-file-name)))
(setq my-elisp-dir (let
		((path (buffer-file-name (car (find-function-noselect 'add-hook)))))
		(substring path 0 (+ 1 (string-match "/lisp/" path)))))
(setq my-vendor-dir (concat my-dotfiles-dir "vendor/"))
(setq my-custom-file-default (concat my-dotfiles-dir "custom.el.default"))
(setq custom-file (concat my-dotfiles-dir "custom.el"))
(if (and (not (file-exists-p custom-file))
		(file-exists-p my-custom-file-default))
	(copy-file my-custom-file-default custom-file))
(add-to-list 'load-path my-dotfiles-dir)
(add-to-list 'load-path my-vendor-dir)

;; Compile when needed
(defun my-recompile-all ()
	(let ((noninteractive-default noninteractive))
		(setq noninteractive t)
		(byte-recompile-directory my-dotfiles-dir 0)
		(setq noninteractive noninteractive-default)))
(my-recompile-all)
(add-hook 'kill-emacs-hook 'my-recompile-all)

;; Autoload
(require 'cl)
(setq generated-autoload-file (concat my-dotfiles-dir "loaddefs.el"))
(update-directory-autoloads my-vendor-dir)
(load generated-autoload-file)

;; Load (no autoload) modules
(autoload 'paredit-mode "paredit"
	"Minor mode for pseudo-structurally editing Lisp code." t)
(require 'show-point-mode)
(require 'saveplace)
(require 'my-screen/my-screen)

;; Helpful modes
(auto-compression-mode t)
(recentf-mode 1)
(show-paren-mode 1)
(ido-mode t)
(color-theme-initialize)
(setq my-color-theme 'color-theme-my-charcoal-black)

;; Default buffer local values
(setq-default fill-column 80)
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq-default save-place t)

;; Seed the random-number generator
(random t)

;; Custom key bindings
(load (concat my-dotfiles-dir "keys"))

;; Mode-specific customizations
(load (concat my-dotfiles-dir "mode-helpers"))
(load (concat my-dotfiles-dir "modes"))

;; System-specific customizations
(load (concat my-dotfiles-dir (symbol-name system-type)) 'noerror)

;; User-specific customizations
(load (concat my-dotfiles-dir "config.el") 'noerror)

;; Custom settings
(load custom-file 'noerror)

;; Color theme
(funcall my-color-theme)
;; If we just load theme here Emacs's win.el overrides face values
;; (http://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434 )
;; so we're loading it again later through `window-setup-hook':
(add-hook 'window-setup-hook my-color-theme)

;; Screen manager
(my-screen-init)