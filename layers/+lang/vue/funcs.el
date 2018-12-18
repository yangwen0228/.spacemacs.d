;;; funcs.el --- vue layer functions file for Spacemacs.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



;; vue functions

(defun vue/tide-detect ()
  "Detect tide binary and warn if not found."
  (let ((found (executable-find "tsserver")))
    (unless found
      (spacemacs-buffer/warning "tide binary not found!"))
    found))

(defun vue/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
