;; Original one works bad with search in lisp code
;; see http://code.google.com/p/yasnippet/issues/detail?id=139
(defun yas/replace-backquotes ()
	"Replace all the \"`(lisp-expression)`\"-style expression
	with their evaluated value"
	(while (re-search-forward yas/backquote-lisp-expression-regexp nil t)
		(let ((current-string (match-string 1)) transformed)
			(delete-region (match-beginning 0) (match-end 0))
			(setq transformed (yas/read-and-eval-string (yas/restore-escapes current-string)))
			(goto-char (match-beginning 0))
			(when transformed (insert transformed)))))
