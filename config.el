;; Font
(set-face-font 'default "-*-Anonymous Pro-*-*-*--11-*-*-*-*-*-*")
;; (set-face-font 'default "-*-Inconsolata-*-*-*--9-*-*-*-*-*-*")
;; (set-face-font 'default "-*-Anonymous-*-*-*--9-*-*-*-*-*-*")

(push "/usr/local/git/bin" exec-path)
(require 'el-kit/html)

(let ((path "~/Documents/org/"))
	(when (file-exists-p path)
		(require 'el-fast-filelist/el-fast-filelist)
		(el-fast-filelist path (kbd "C-;"))))
