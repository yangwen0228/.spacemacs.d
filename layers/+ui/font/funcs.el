;;; funcs.el --- ui layer functions file for Spacemacs.
;;
;; Copyright (c) 2018 WEN YANG & Contributors
;;
;; Author: WEN YANG <steven@WENdeMacBook-Pro.local>
;; URL: https://github.com/yangwen0228/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; ui

(defun ui//set-font (size)
  "Main function to set font by english font size: SIZE."
  (setq ui--current-english-font-size
        (or size ui-default-english-font-size))

  ;; init english font
  (unless ui--current-english-font
    ;; for find-if
    (require 'cl)
    (setq ui--current-english-font
          (find-if #'ui//font-existp ui-english-candidate-fonts)))

  ;; Set English font
  ;; NOTE:
  ;; The following 2 methods not work in new frames.
  ;; (set-default-font "Consolas:pixelsize=18")
  ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
  ;; Have to use `set-face-attribute'.
  (set-face-attribute 'default nil
                      :font (ui//make-font-string
                             ui--current-english-font
                             ui--current-english-font-size))

  ;; init chinese font
  (unless ui--current-chinese-font
    ;; for find-if
    (require 'cl)
    (setq ui--current-chinese-font
          (find-if #'ui//font-existp ui-chinese-candidate-fonts)))

  ;; Set Chinese font
  (setq ui--current-chinese-scale
        (or (cdr (assoc size ui-chinese-font-size-scale-alist)) 1.0))
  ;; override variable
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset
                      (font-spec :family ui--current-chinese-font
                                 :size (* ui--current-chinese-scale
                                          ui--current-english-font-size))))
  (message "Your font size is set to %.1f" ui--current-english-font-size))

(defun ui//font-existp (font)
  (if (null (x-list-fonts font))
      nil
    t))

(defun ui//make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s-%s" font-name font-size)))
