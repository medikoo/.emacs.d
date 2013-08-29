;; Font
(set-face-font 'default "-*-Anonymous Pro-*-*-*--11-*-*-*-*-*-*")
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
