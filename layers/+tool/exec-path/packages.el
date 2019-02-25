;;; packages.el --- exec-path layer packages file for Spacemacs.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; exec-path

;;; Code:

(setq exec-path-packages
      '(exec-path-from-shell))

(defun exec-path/post-init-exec-path-from-shell ()
  (cond
   ((spacemacs/system-is-mac) (add-to-list 'exec-path "/usr/local/Cellar/node/11.4.0/bin"))
   ((spacemacs/system-is-mswindows)
    (add-to-list 'exec-path "C:/git/unimacs/utils/extra-bins/msys64/bin")
    (setenv "PATH" (concat
                    (replace-regexp-in-string "\/" "\\\\" "C:/git/unimacs/utils/extra-bins/msys64/bin")
                    ";"
                    (getenv "PATH")))
    )
   ))





;;; packages.el ends here
