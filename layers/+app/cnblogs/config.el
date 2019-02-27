;;; config.el --- cnblogs layer configuration file for Spacemacs.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; cnblogs

(defvar cnblogs-root-dir "~/Documents/orgs/cnblogs/"
  "The root directory of cnblogs .org files.")

(defvar cnblogs-store-dir nil
  "The absolute configuration directory of cnblogs.

Contain the publish records. Putting this dir under `cnblogs-root-dir' is recommended.")

(defvar cnblogs-relative-store-dir ".cnblogs/"
  "The relative configuration directory according to `cnblogs-root-dir'.

It works only when `cnblogs-store-dir' is nil.")
