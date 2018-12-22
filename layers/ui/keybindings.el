;; override should put in keybindings, load at last.
(defun spacemacs/scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (if (= direction 0)
      (ui//set-font ui-default-english-font-size)
    (let ((sizes (if (> direction 0)
                     ui-english-font-sizes
                   (reverse ui-english-font-sizes)))
          size)
      (setq sizes (member ui--current-english-font-size sizes))
      (unless sizes
        (error "The current size %s is not in `ui-english-font-sizes'." ui--current-english-font-size))
      (setq size (if (cadr sizes) (cadr sizes) (car sizes)))
      (ui//set-font size))))

(spacemacs/scale-up-or-down-font-size 0)
