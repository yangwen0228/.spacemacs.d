;;; happie-jump.el --- Try multiple 'jump to source' commands for the current mode

;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:

;; It's inspired by https://github.com/rmloveland/omni-jump/blob/master/omni-jump.el

;; This package tries to unite the various 'jump to definition'
;; commands you use so they all use the same keybinding, regardless of
;; language or mode.

;; It exports one function, `happie-jump', and requires you to fill in
;; an alist, `happie-jump-try-definition-functions-alist', which will look something
;; like this (here's mine).

;;        (setq happie-jump-try-definition-functions-alist
;;              '((confluence-markup-mode . (confluence-markup-visit-wiki-word-file-at-point))
;;                (c-mode . (xref-find-definitions ggtags-find-tag-dwim semantic-ia-fast-jump))
;;                (cperl-mode . (cperl-view-module-source))
;;                (scheme-mode . (xref-find-definitions))
;;                (emacs-lisp-mode . (elisp-slime-nav-find-elisp-thing-at-point))
;;                (lisp-interaction-mode . (elisp-slime-nav-find-elisp-thing-at-point))))

;;; Code:

(defvar happie-jump-try-definition-functions-alist nil
  "Goto definition by `happie-jump-definition' to decide
which jump function to use for a given mode.")

(defvar happie-jump-try-references-functions-alist nil
  "Goto definition by `happie-jump-references' to decide
which jump function to use for a given mode.")

(defvar happie-jump-default-definition-functions nil
  "If the major-mode's jump definition functions is configured in
`happie-jump-try-definition-functions-alist', use this function instead.")

(defvar happie-jump-default-references-functions nil
  "If the major-mode's jump references functions is configured in
`happie-jump-try-references-functions-alist', use this function instead.")

(defun happie-jump--get-definition-functions (mode)
  "Given the name of a MODE, return the jump definition functions to call."
  (seq-filter 'fboundp
              (or (assoc mode happie-jump-try-definition-functions-alist)
                  happie-jump-default-definition-functions)))

(defun happie-jump--get-references-functions (mode)
  "Given the name of a MODE, return the jump references functions to call."
  (seq-filter 'fboundp
              (or (assoc mode happie-jump-try-references-functions-alist)
                  happie-jump-default-references-functions)))

(defun happie-jump-definition ()
  "Call the 'jump to source' functions defined for the current mode."
  (interactive)
  (let* ((mode major-mode)
         (funcs (happie-jump--get-definition-functions mode)))
    (if (or (null funcs)
            (not (listp funcs))
            (= 0 (length funcs)))
        (error
         "No handler for `%s' defined in `happie-jump-try-definition-functions-alist'"
         mode)
      (let ((i 0)
            (len (length funcs)))
	      (while (not (or (>= i len)
			                  (when (fboundp (nth i funcs))
                          (call-interactively (nth i funcs)))))
	        (setq i (1+ i)))))))

(defun happie-jump-references ()
  "Call the 'jump to references' functions defined for the current mode."
  (interactive)
  (let* ((mode major-mode)
         (funcs (happie-jump--get-references-functions mode)))
    (if (or (null funcs)
            (not (listp funcs))
            (= 0 (length funcs)))
        (error
         "No handler for `%s' defined in `happie-jump-try-references-functions-alist'"
         mode)
      (let ((i 0)
            (len (length funcs)))
	      (while (not (or (>= i len)
			                  (when (fboundp (nth i funcs))
                          (call-interactively (nth i funcs)))))
	        (setq i (1+ i)))))))

(provide 'happie-jump)

;;; happie-jump.el ends here
