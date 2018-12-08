;;; packages.el --- better-editing layer packages file for Spacemacs.
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

;; custom settings for editing.

;;; Code:

(setq better-editing-packages
      '(
        ;; init
        multiple-cursors
        number
        whole-line-or-region
        yasnippet-snippets
        ;; post
        company
        helm
        helm-swoop
        imenu
        winum
        yasnippet
        ))

(defun better-editing/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :bind* (("C-;"     . mc/mark-all-like-this-dwim)
            ("C-:"     . mc/mark-all-like-this-in-defun-dwim)
            ("C->"     . mc/mark-next-like-this)
            ("C-<"     . mc/mark-previous-like-this)
            ("C-M->"   . mc/skip-to-next-like-this)
            ("C-M-<"   . mc/skip-to-previous-like-this)
            ("C-S-<mouse-1>" . mc/add-cursor-on-click)
            :map mc/keymap
            ("C-|" . mc/vertical-align-with-space)
            ("C-_" . undo) ;undo-tree-undo point position wrong.
            ("M-n" . mc/cycle-forward)
            ("M-p" . mc/cycle-backward))
    :init
    (progn
      (require 'multiple-cursors)
      (setq mc/list-file better-editing/mc-config-file
            mc/insert-numbers-default 1
            mc/cycle-looping-behaviour 'stop))
    :config
    (progn
      (defun mc/my-quit ()
        "Quit from mark mode."
        (interactive)
        (mc/keyboard-quit)
        (multiple-cursors-mode 0))

      (defun mc/mark-all-symbols-like-this-toggle ()
        "Toogle when only one matches!"
        (interactive)
        (if (or multiple-cursors-mode (region-active-p))
            (mc/my-quit)
          (mc/mark-all-symbols-like-this)))

      (defun mc/mark-all-like-this-dwim ()
        "Toggle when not using region. When using region, search first,
if only one candidate searched, then quit!"
        (interactive)
        (if multiple-cursors-mode
            (mc/my-quit)
          (if (not (region-active-p))
              (mc/mark-all-symbols-like-this)
            (mc/mark-all-like-this)
            (unless multiple-cursors-mode
              (mc/my-quit)))))
      (defun mc/mark-all-like-this-in-defun-dwim ()
        "Like `mc/mark-all-like-this-dwim', but only in defun."
        (interactive)
        (if multiple-cursors-mode
            (mc/my-quit)
          (if (not (region-active-p))
              (mc/mark-all-symbols-like-this-in-defun)
            (mc/mark-all-like-this-in-defun)
            (unless multiple-cursors-mode
              (mc/my-quit))))))))

(defun better-editing/init-number ()
  (use-package number
    :defer t
    :commands mc/number/add mc/number/divide mc/number/multiply
    number/add number/sub number/multiply number/divide number/eval
    ))

(defun better-editing/init-whole-line-or-region ()
  (use-package whole-line-or-region
    ;; kill or yank a whole line.
    :defer t
    :diminish whole-line-or-region-mode))

(defun better-editing/init-yasnippet-snippets ()
  (when (configuration-layer/package-usedp 'yasnippet)
    (use-package yasnippet-snippets
      :defer t
      )))

(defun better-editing/post-helm-swoop ()
  (setq helm-swoop-move-to-line-cycle nil)
  (bind-key* "M-i" 'spacemacs/helm-swoop-region-or-symbol))

(defun better-editing/post-winum ()
  (better-editing/defadvice-commands
   "auto-save" before
   (winum-select-window-1
    winum-select-window-2
    winum-select-window-3
    winum-select-window-4
    winum-select-window-5)
   (better-editing/auto-save)))

(defun better-editing/post-imenu ()
  (add-to-list 'lisp-imenu-generic-expression
               (list "Use-package"
                     (concat
                      "^\\s-*(use-package\\s-+" ; definition
                      "\\([-A-Za-z0-9_:+*]+\\)" ; package name
                      )
                     1)))

(defun better-editing/post-company ()
  )


;;; packages.el ends here
