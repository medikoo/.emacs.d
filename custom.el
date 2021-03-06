(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-command "ag ")
 '(auto-mode-case-fold t)
 '(auto-save-file-name-transforms
	 (list
		 (list ".*"
			 (concat estarter-dotfiles-dir "auto-save-list/")
t)))
 '(backup-directory-alist (list (cons "." (concat estarter-dotfiles-dir "backups/"))))
 '(backward-delete-char-untabify-method nil)
 '(bookmark-default-file (concat estarter-dotfiles-dir ".emacs.bmk"))
 '(css-indent-offset estarter-tab-width)
 '(delete-by-moving-to-trash t)
 '(echo-keystrokes 0.1)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 100)
 '(grep-command "grep -nHre ")
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(imenu-auto-rescan t)
 '(indent-tabs-mode estarter-indent-tabs-mode)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote emacs-lisp-mode))
 '(ispell-program-name "aspell")
 '(js-enabled-frameworks (quote (javascript)))
 '(js-indent-level 2)
 '(js2-basic-offset estarter-tab-width)
 '(js2-highlight-level 3)
 '(js2-mirror-mode nil)
 '(line-number-display-limit-width 9999)
 '(message-log-max t)
 '(mode-require-final-newline nil)
 '(mouse-wheel-mode t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(org-startup-truncated nil)
 '(python-guess-indent nil)
 '(python-indent estarter-tab-width)
 '(ruby-indent-tabs-mode estarter-indent-tabs-mode)
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
	 (quote
		 ((js2-basic-offset . 4)
			 (js2-basic-offset . 2)
			 (js2-basic-offset . 8)
			 (js2-strict-missing-semi-warning)
			 (js2-strict-missing-semi-warning . t))))
 '(save-place t nil (saveplace))
 '(save-place-file (concat estarter-dotfiles-dir "places"))
 '(shift-select-mode nil)
 '(tab-stop-list
	 (let
		 ((list
				 (list estarter-tab-width)))
		 (while
			 (<
				 (car
					 (last list))
				 200)
			 (nconc list
				 (list
					 (+ estarter-tab-width
						 (car
(last list))))))
list))
 '(tab-width estarter-tab-width)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-make-backup-files t)
 '(visible-bell t)
 '(whitespace-line-column fill-column)
 '(whitespace-style
	 (quote
		 (face trailing lines-tail indentation space-after-tab space-before-tab))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
