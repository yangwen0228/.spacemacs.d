;;; packages.el --- cnblogs layer packages file for Spacemacs.
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

;; custom settings for cnblogs.

;;; Code:

(setq cnblogs-packages
      '(
        xml-rpc
        (cnblogs :location local)
        ))

(defun cnblogs/init-xml-rpc ()
  (use-package xml-rpc
    :defer t
    ))

(defun cnblogs/init-cnblogs ()
  (use-package cnblogs :ensure nil
    ;; Run command: cnblogs/setup-blog to set up, blog-id == username.
    :defer t
    :commands (cnblogs/setup-blog
               cnblogs/new-or-update-post
               cnblogs/delete-post)
    :init
    (setq cnblogs-store-root-dir (if cnblogs-store-dir
                                     cnblogs-store-dir
                                   (expand-file-name
                                    cnblogs-relative-store-dir
                                    cnblogs-root-dir)))
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "Bs" 'cnblogs/setup-blog
      "Bp" 'cnblogs/new-or-update-post
      "Bd" 'cnblogs/delete-post)))

;;; packages.el ends here
