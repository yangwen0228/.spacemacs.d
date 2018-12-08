;;; funcs.el --- web-tool layer functions file for Spacemacs.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; web-tool

(defconst restclient-example-http-file
  (expand-file-name
   "example.http"
   (file-name-directory
    ;; Copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))

(defun web-tool/restclient-open-example-http ()
  "Open the example.http file in other buffer."
  (interactive)
  (view-file-other-window restclient-example-http-file))
