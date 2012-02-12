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
	(dir-locals-set-class-variables 'node-jslint
		'((nil . ((indent-tabs-mode . nil)
					(js2-basic-offset . 4)))))
	(dir-locals-set-directory-class
		"~/Projects/_packages/jslint/" 'node-jslint))

(when (fboundp 'dir-locals-set-class-variables)
	(dir-locals-set-class-variables 'bloodhound
		'((nil . ((indent-tabs-mode . nil)
					(js2-basic-offset . 2)))))
	(dir-locals-set-directory-class
		"~/Projects/_packages/bloodhound/" 'bloodhound))
