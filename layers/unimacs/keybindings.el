;;; keybindings.el --- unimacs layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



(when (configuration-layer/layer-usedp 'better-editing)
  ;;  line editing
  (bind-key* "C-w" 'whole-line-or-region-kill-region)
  (bind-key* "M-w" 'whole-line-or-region-kill-ring-save)
  (bind-key* "C-y" 'whole-line-or-region-yank)
  ;; copy/yank word
  (bind-key* "C-M-w" 'better-editing/copy-word)
  (bind-key* "C-M-y" 'better-editing/replace-word)
  ;; join lines
  (bind-key* "M-j" 'better-editing/join-next-line)
  (bind-key* "M-J" 'better-editing/join-to-previous-line)
  ;; open line
  (bind-key* "C-o" 'better-editing/open-line-below)
  (bind-key* "C-S-o" 'better-editing/open-line-above)
  ;; camel/underscore
  (spacemacs/set-leader-keys
    "x_" 'better-editing/camel-to-underscore
    "x-" 'better-editing/underscore-to-camel)
  ;; helm
  (when (configuration-layer/package-usedp 'helm)
    (bind-key* "C-x C-f" 'helm-find-files))
)
