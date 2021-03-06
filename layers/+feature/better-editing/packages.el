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
        server
        ;; smart-tab
        jumplist
        (happie-jump :location local)
        ;; post
        ivy
        company
        elisp-slime-nav
        imenu
        helm
        magit
        winum
        undo-tree
        smartparens
        helm-swoop
        helm-ag
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
            ("C-；"     . mc/mark-all-like-this-dwim)
            ("C-："     . mc/mark-all-like-this-in-defun-dwim)
            ("C-》"     . mc/mark-next-like-this)
            ("C-《"     . mc/mark-previous-like-this)
            ("C-M-》"   . mc/skip-to-next-like-this)
            ("C-M-《"   . mc/skip-to-previous-like-this)
            ("C-S-<mouse-1>" . mc/add-cursor-on-click)
            :map mc/keymap
            ("C-|" . mc/vertical-align-with-space)
            ("C-_" . undo) ;undo-tree-undo point position wrong.
            ("C-—" . undo) ;chinese
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
      :init
      (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
      )))

(defun better-editing/init-server ()
  (use-package server :ensure nil
    :init
    (defun server-ensure-safe-dir (dir) "Noop" t)
    (unless (file-exists-p server-auth-dir)
      (make-directory server-auth-dir))
    (unless (or (not server-socket-dir) (file-exists-p server-socket-dir))
      (make-directory server-socket-dir))))

;; (defun better-editing/init-smart-tab ()
;;   (use-package smart-tab
;;     :defer t
;;     :diminish smart-tab-mode
;;     :init
;;     (setq smart-tab-completion-functions-alist nil
;;           smart-tab-using-hippie-expand t)
;;     (if (configuration-layer/package-usedp 'org)
;;         (add-hook 'org-mode-hook 'smart-tab-mode-on))
;;     ;; bug in `smart-tab-default', the default keybinding can't be `smart-tab'
;;     ;; avoid infinite looping, can't use `bind*'. use `add-hook' to turn on.
;;     :bind ("<tab>" . smart-tab)
;;     :config
;;     (setq smart-tab-disabled-major-modes
;;           (remove 'org-mode smart-tab-disabled-major-modes)) ; org-mode: yasnippet
;;     (global-smart-tab-mode 1)))

(defun better-editing/init-jumplist ()
  (use-package jumplist
    :defer 0
    :bind* (("<C-M-left>" . jumplist-previous)
            ("<C-M-right>" . jumplist-next))
    :init
    (custom-set-variables
     '(jumplist-hook-commands
       '(beginning-of-defun
         end-of-defun
         end-of-buffer beginning-of-buffer
         happie-jump-definition
         happie-jump-references
         helm-swoop helm-imenu helm-find-files helm-multi-files
         helm-projectile-switch-project helm-projectile-find-file
         find-function find-variable))
     '(jumplist-ex-mode t))
    ))

(defun better-editing/init-happie-jump ()
  (use-package happie-jump :ensure nil
    :defer t
    :bind* (("M-." . happie-jump-definition)
            ("M-?" . happie-jump-references))
    :init
    (setq happie-jump-try-definition-functions-alist
          '((emacs-lisp-mode . (elisp-slime-nav-find-elisp-thing-at-point))))
    (setq happie-jump-try-references-functions-alist
          '((emacs-lisp-mode . (better-editing/xref-find-references))))
    (when (configuration-layer/package-usedp 'dumb-jump)
      (setq happie-jump-default-definition-functions '(dumb-jump-go))
      (setq happie-jump-default-references-functions '(spacemacs/helm-project-do-ag-region-or-symbol)))
    :config
    (when (configuration-layer/package-usedp 'jumplist)
      (bind-key* "M-," 'jumplist-previous))))

;; post

(defun better-editing/post-init-ivy ()
  (with-eval-after-load 'ivy
    (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory)
    (define-key ivy-minibuffer-map (kbd "C-l") 'counsel-up-directory)
    (define-key counsel-find-file-map (kbd "C-j") 'ivy-alt-done)
    (defun better-editing//swiper-region-or-symbol ()
      "Call `swiper' with default input."
      (interactive)
      (swiper (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (let ((thing (thing-at-point 'symbol t)))
                  (if thing thing "")))))
    (global-set-key (kbd "M-i") 'better-editing//swiper-region-or-symbol)
    ))

(defun better-editing/post-init-winum ()
  (better-editing/defadvice-commands
   "auto-save" before
   (winum-select-window-1
    winum-select-window-2
    winum-select-window-3
    winum-select-window-4
    winum-select-window-5)
   (better-editing/auto-save)))

(defun better-editing/post-init-imenu ()
  (add-to-list 'lisp-imenu-generic-expression
               (list "Use-package"
                     (concat
                      "^\\s-*(use-package\\s-+" ; definition
                      "\\([-A-Za-z0-9_:+*]+\\)" ; package name
                      )
                     1)))

(defun better-editing/post-init-company ()
  (setq company-show-numbers t))

(defun better-editing/post-init-elisp-slime-nav ()
  (defun better-editing/xref-find-references (identifier)
    (interactive (list (elisp-slime-nav--read-symbol-at-point)))
    (xref--find-xrefs identifier 'references identifier nil))
  (bind-key "M-?" 'better-editing/xref-find-references))

(defun better-editing/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (progn
      (unbind-key "M-1" magit-mode-map)
      (unbind-key "M-2" magit-mode-map)
      (unbind-key "M-3" magit-mode-map)
      (unbind-key "M-4" magit-mode-map))))

(defun better-editing/post-init-undo-tree ()
  )

(defun better-editing/post-init-helm ()
  (defun helm-ff-directory-files (directory)
  "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but always returns the
dotted filename '.' and '..' even on root directories in Windows
systems."
  (setq directory (file-name-as-directory
                   (expand-file-name directory)))
  (let* (file-error
         (ls   (condition-case err
                   (helm-list-directory directory)
                 ;; Handle file-error from here for Windows
                 ;; because predicates like `file-readable-p' and friends
                 ;; seem broken on emacs for Windows systems (always returns t).
                 ;; This should never be called on GNU/Linux/Unix
                 ;; as the error is properly intercepted in
                 ;; `helm-find-files-get-candidates' by `file-readable-p'.
                 (file-error
                  (prog1
                      (list (format "%s:%s"
                                    (car err)
                                    (mapconcat 'identity (cdr err) " ")))
                    (setq file-error t)))))
         (dot  (concat directory "."))
         (dot2 (concat directory "..")))
    (puthash directory (+ (length ls) 2) helm-ff--directory-files-hash)
    ;; (append (and (not file-error) (list dot dot2)) ls)
    ;; return the files only, excluding the dot fiels "." and "..".
    ls
    )))

(defun better-editing/post-init-helm-swoop ()
  (setq helm-swoop-move-to-line-cycle nil)
  (bind-key* "M-i" 'spacemacs/helm-swoop-region-or-symbol)
  (better-editing/defadvice-commands
   "auto-save" before
   (helm-swoop-edit)
   (better-editing/auto-save)))

(defun better-editing/post-init-helm-ag ()
  (better-editing/defadvice-commands
   "auto-save" before
   (helm-ag-edit)
   (better-editing/auto-save)))

(defun better-editing/post-init-smartparens ()
  ;; forward/backward
  (bind-key* "C-M-f" 'sp-forward-sexp)
  (bind-key* "C-M-b" 'sp-backward-sexp)
  (bind-key* "C-M-k" 'kill-sexp)
  (bind-key* "C-M-SPC" 'sp-mark-sexp)
  (bind-key* "C-M-u" 'sp-backward-up-sexp)
  (bind-key* "C-M-d" 'sp-down-sexp)
  (bind-key* "M-[" 'sp-unwrap-sexp)
  (bind-key* "M-]" 'sp-rewrap-sexp)
  )

;;; packages.el ends here
