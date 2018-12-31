;;; packages.el --- vue layer packages file for Spacemacs.
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

;; vue major mode

;;; Code:

(setq vue-packages
      '(
        ;; init
        vue-mode
        nodejs-repl
        (javascript-mode :location built-in)
        ;; post
        company
        ;; tide
        ;; company-tide
        dumb-jump
        tern
        company-tern
        flycheck
        js-doc
        web-beautify
        yasnippet
        ))

(defun vue/init-vue-mode ()
  (use-package vue-mode
    :defer t
    :mode (("\\.vue\\'" . vue-mode))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'vue-mode
      "r" 'vue-mode-reparse
      "e" 'vue-mode-edit-indirect-at-point)
    (setq vue-html-extra-indent 0
          sgml-basic-offset 2
          js-indent-level 2
          css-indent-offset 2)
    (modify-syntax-entry ?. ".")))

(defun vue/init-nodejs-repl ()
  (use-package nodejs-repl
    :defer t
    :commands nodejs-repl
    :bind (("C-c C-t" . nodejs-repl)
           ("C-c C-v" . nodejs-repl-send-region)
           ("C-c C-b" . nodejs-repl-send-buffer))))

(defun vue/init-javascript-mode ())

;; post

(defun vue/post-init-company ()
  (spacemacs|add-company-hook js-mode)
  (spacemacs|add-company-hook vue-mode))

(defun vue/post-init-dumb-jump ()
  (defvar vue--init-dumb-jump-initp nil)
  (unless vue--init-dumb-jum-initp
    (setq vue--init-dumb-jump-initp t)
    (add-to-list
     'dumb-jump-find-rules
     '(:type "variable"
             :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
             :regex "\\s*\\bJJJ\\s*:[^:\\n]+"
             :tests ("test : 1234", "test: 1234")))))

(defun vue/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'js-mode)
  (spacemacs/add-flycheck-hook 'vue-mode)
  (add-hook 'flycheck-mode-hook (lambda()
                                  (flycheck-add-mode 'javascript-eslint 'js-mode)
                                  (vue/use-eslint-from-node-modules)
                                  (flycheck-select-checker 'javascript-eslint))))

(defun vue/post-init-js-doc ()
  (spacemacs/js-doc-set-key-bindings 'js-mode)
  (spacemacs/js-doc-set-key-bindings 'vue-mode))

;; (defun vue/post-init-tide ()
;;   (add-hook 'js-mode-hook 'tide-setup)
;;   (push 'company-tide company-backends-js-mode))

(defun vue/post-init-company-tern ()
  (when (and (configuration-layer/package-usedp 'company)
             (configuration-layer/package-usedp 'tern))
    (add-to-list 'company-backends-js-mode 'company-tern)
    (add-to-list 'company-backends-vue-mode 'company-tern)))

(defun vue/post-init-tern ()
  ;; cnpm -g install tern
  (add-hook 'js-mode-hook 'tern-mode)
  (add-hook 'vue-mode-hook 'tern-mode))

(defun vue/post-init-web-beautify ()
  ;; cnpm -g install js-beautify
  (spacemacs/set-leader-keys-for-major-mode 'js-mode
    "=" 'web-beautify-js))

(defun vue/post-init-yasnippet ()
  (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(js-mode-hook vue-mode-hook)))


;;; packages.el ends here
