
(defvar ui-default-english-font-size nil
  "Default English font size.

Chinese font size is automatically adjusted according to
 `ui-chinese-font-size-scale-alist'")

(defvar ui-english-font-sizes nil
  "English font size list.")

(defvar ui-english-candidate-fonts nil
  "Canditate English font list.")

(defvar ui-chinese-candidate-fonts nil
  "Canditate Chinese font list.")

(defvar ui-chinese-font-size-scale-alist nil
  "Chinese font size is scaled to English font size.

So that one Chinese char's width is equal to two English chars' width.")
(cond
 ((spacemacs/system-is-mac)
  (setq ui-default-english-font-size 11)
  (setq ui-english-font-sizes
        '(9 10 11 12 14 16 18 20))
  (setq ui-chinese-font-size-scale-alist
        '((9 . 1.1) (10 . 1.2) (11 . 1.3) (12 . 1.2) (14 . 1.18)
          (16 . 1.25) (18 . 1.2) (20 . 1.2)))
  (setq ui-english-candidate-fonts '("Menlo"))
  (setq ui-chinese-candidate-fonts '("华文黑体")))
 ((spacemacs/system-is-mswindows)
  (setq ui-default-english-font-size 12)
  (setq ui-english-font-sizes
        '(9 10 11 12 14 16 18 20))
  (setq ui-chinese-font-size-scale-alist
        '((9 . 1.1) (10 . 1.1) (11 . 1.1) (12 . 1.1) (14 . 1.08)
          (16 . 1.12) (18 . 1.12) (20 . 1.1)))
  (setq ui-english-candidate-fonts '("Menlo" "Consolas"))
  (setq ui-chinese-candidate-fonts '("Microsoft Yahei" "微软雅黑")))
 (t ;; is a linux
  ))

;; private variables
(defvar ui--current-english-font-size nil)
(defvar ui--current-chinese-scale nil)
(defvar ui--current-english-font nil)
(defvar ui--current-chinese-font nil)
