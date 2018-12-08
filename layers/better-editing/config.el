;;; config.el --- better-editing layer configuration file for Spacemacs.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; better-editing

(defvar better-editing/mc-config-file
  (expand-file-name "mc-config.el" spacemacs-cache-directory)
  "Multiple cursors configuration file.")

;; advise all window switching functions
(better-editing/defadvice-commands
 "auto-save" before
 (switch-to-buffer other-window)
 (better-editing/auto-save))
(add-hook 'mouse-leave-buffer-hook 'better-editing/auto-save)
(add-hook 'focus-out-hook 'better-editing/auto-save)
