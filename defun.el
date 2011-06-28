;; defun.el --- Useful methods for emacs
;;
;; Author:	Mariusz Nowak <mariusz+emacs@medikoo.com>
;; Copyright (C) 2011 Mariusz Nowak <mariusz+emacs@medikoo.com>

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
;; Useful methods

(defun estarter-scratch ()
	"Create new scratch buffer and switch to it."
	(interactive)
	(let* (
			(mode (symbol-name major-mode))
			(mode (substring mode 0 (el-kit-string-last-index-of mode "-")))
			(index 1)
			(make-name
				(lambda () (concat "*" mode "[" (number-to-string index) "]*")))
			(name (funcall make-name)))
		(while (get-buffer name)
			(setq index (+ index 1))
			(setq name (funcall make-name)))
		(switch-to-buffer name)
		(funcall (intern (concat mode "-mode")))))
