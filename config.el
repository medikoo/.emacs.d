;; Font
(set-face-font 'default "-*-Anonymous Pro-*-*-*--26-*-*-*-*-*-*")
;; (set-face-font 'default "-*-Inconsolata-*-*-*--9-*-*-*-*-*-*")
;; (set-face-font 'default "-*-Anonymous-*-*-*--9-*-*-*-*-*-*")

(el-screen-set-frame-title)

(push "/usr/local/git/bin" exec-path)
(require 'el-kit/html)

(let ((path "~/Documents/org/"))
	(when (file-exists-p path)
		(require 'el-fast-filelist/el-fast-filelist)
		(el-fast-filelist path (kbd "C-;"))))

(when (fboundp 'dir-locals-set-class-variables)
	(dir-locals-set-class-variables 'impressjs
		'((nil . ((indent-tabs-mode . nil)
					(js2-basic-offset . 4)))))
	(dir-locals-set-directory-class
		"~/Projects/impress.js/" 'impressjs))

(when (fboundp 'dir-locals-set-class-variables)
	(dir-locals-set-class-variables 'nodejs
		'((nil . ((indent-tabs-mode . nil)
					(js2-basic-offset . 2)))))
	(dir-locals-set-directory-class
		"~/.nvm/src/" 'nodejs))

(when (fboundp 'dir-locals-set-class-variables)
	(dir-locals-set-class-variables 'forks
		'((nil . ((indent-tabs-mode . nil)
					(js2-basic-offset . 2)))))
	(dir-locals-set-directory-class
		"~/Projects/_forks/" 'forks))

(when (fboundp 'dir-locals-set-class-variables)
	(dir-locals-set-class-variables 'soundcloud
		'((nil . ((indent-tabs-mode . nil)
					(js2-basic-offset . 2)))))
	(dir-locals-set-directory-class
		"~/Projects/soundcloud-playlist-manager" 'soundcloud))

(when (fboundp 'dir-locals-set-class-variables)
	(dir-locals-set-class-variables 'npm
		'((nil . ((indent-tabs-mode . nil)
					(js2-basic-offset . 2)))))
	(dir-locals-set-directory-class
		"~/Projects/npm" 'npm))

(when (fboundp 'dir-locals-set-class-variables)
	(dir-locals-set-class-variables 'querystring
		'((nil . ((indent-tabs-mode . nil)
					(js2-basic-offset . 2)))))
	(dir-locals-set-directory-class
		"~/Projects/_packages/querystring" 'querystring))

(defun estarter-js2-jslintjs ()
	"Different whitespace settings"
	(when (string-equal js2-buffer-file-name
			"/Users/medikoo/Projects/_packages/jslint/lib/jslint.js")
		(setq indent-tabs-mode nil)
		(setq js2-basic-offset 4)))
(add-hook 'estarter-js2-mode-hook 'estarter-js2-jslintjs)

(defun set-exec-path-from-shell-PATH ()
	(let ((path-from-shell (replace-regexp-in-string
					"[ \t\n]*$"
					""
					(shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
		(setenv "PATH" path-from-shell)
		(setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

(when (load "flymake" t)
	(defun flymake-jslint-init ()
		(if (eq (string-match
					"/Users/medikoo/Projects/\\(eregistrations\\|_packages\\|_forks\\|presentations\\)"
					buffer-file-name) 0)
			(let* ((temp-file (flymake-init-create-temp-buffer-copy
							'flymake-create-temp-inplace))
					(local-file (file-relative-name
							temp-file
							(file-name-directory buffer-file-name))))
				(list
					"/Users/medikoo/Projects/_packages/xlint/bin/xlint"
					(list
						"--linter=/Users/medikoo/Projects/_packages/xlint-jslint-medikoo/index.js"
						(concat "--realFilename=" buffer-file-name)
						"--terse" "--no-cache" temp-file)))))

	(setq flymake-err-line-patterns
		(cons '("^\\(.*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
				1 2 3 4)
			flymake-err-line-patterns))

	(add-to-list 'flymake-allowed-file-name-masks
		'("\\.js\\'" flymake-jslint-init))

	(require 'flymake-cursor)

	;; ;; Prevent Flymake crashing
	;; Not needed with Emacs > 24.3
	;; (defun flymake-post-syntax-check (exit-status command)
	;; 	(setq flymake-err-info flymake-new-err-info)
	;; 	(setq flymake-new-err-info nil)
	;; 	(setq flymake-err-info
	;; 		(flymake-fix-line-numbers
	;; 			flymake-err-info 1 (flymake-count-lines)))
	;; 	(flymake-delete-own-overlays)
	;; 	(flymake-highlight-err-lines flymake-err-info)
	;; 	(let (err-count warn-count)
	;; 		(setq err-count (flymake-get-err-count flymake-err-info "e"))
	;; 		(setq warn-count  (flymake-get-err-count flymake-err-info "w"))
	;; 		(flymake-log 2 "%s: %d error(s), %d warning(s) in %.2f second(s)"
	;; 			(buffer-name) err-count warn-count
	;; 			(- (flymake-float-time) flymake-check-start-time))
	;; 		(setq flymake-check-start-time nil)

	;; 		(if (and (equal 0 err-count) (equal 0 warn-count))
	;; 			(if (equal 0 exit-status)
	;; 				(flymake-report-status "" "")	; PASSED
	;; 				(if (not flymake-check-was-interrupted)
	;; 					(progn (message
	;; 							(format "Configuration error has occurred while running %s" command))
	;; 						(flymake-mode)
	;; 						(flymake-mode))
	;; 					(flymake-report-status nil ""))) ; "STOPPED"
	;; 			(flymake-report-status (format "%d/%d" err-count warn-count) ""))))

)

(add-hook 'js-mode-hook
	(lambda ()
		(flymake-mode 1)
		(define-key js2-mode-map "\C-c\C-n" 'flymake-goto-next-error)))
