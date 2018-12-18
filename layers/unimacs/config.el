;;; config.el --- unimacs layer config file for Spacemacs.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



;; unimacs config

(setq frame-title-format
      '(" Unimacs - "
        (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
