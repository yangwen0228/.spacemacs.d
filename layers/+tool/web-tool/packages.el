;;; packages.el --- web-tool layer packages file for Spacemacs.
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

;; web-tool tools

;;; Code:

(defconst web-tool-packages
  '(
    restclient
    ))

(defun web-tool/post-restclient ()
  (spacemacs/set-leader-keys-for-major-mode 'restclient-mode
    "e" 'web-tool/restclient-open-example-http))


;;; packages.el ends here
