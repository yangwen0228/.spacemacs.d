;;; funcs.el --- better-editing layer functions file for Spacemacs.
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

(defun better-editing/copy-word ()
  (interactive)
  (save-excursion
    (let* ((b (if (region-active-p)
                  (region-bounds)
                (bounds-of-thing-at-point 'symbol)))
           (beg (car b))
           (end (cdr b)))
      (copy-region-as-kill beg end))))

(defun better-editing/replace-word ()
  (interactive)
  (better-editing/replace-string (current-kill 0)))

(defun better-editing/replace-string (new)
  "Replace the string from beg to end by NEW."
  (let* ((b (if (region-active-p)
                (cons (region-beginning) (region-end))
              (bounds-of-thing-at-point 'symbol)))
         (beg (car b))
         (end (cdr b))
         (old (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol t))))
    (goto-char beg)
    (search-forward old end 't 1)
    (replace-match new)))

(defun better-editing/join-lines ()
  "Join whole buffer or region."
  (interactive)
  (let* ((positions (better-editing/get-positions-of-buffer-or-region))
         (beg (car positions))
         (end (cdr positions)))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (search-forward-regexp "[ \t]*\n[ \t]*" nil t))
        (replace-match "")))))

(defun better-editing/join-next-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun better-editing/join-to-previous-line ()
  "Join the current line to the line above it."
  (interactive)
  (delete-indentation))

(defun better-editing/get-positions-of-buffer-or-region ()
  "Return positions (beg . end) of the current buffer or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (if mark-active
        (setq beg (line-beginning-position))
      (setq beg (point-min)))
    (if mark-active
        (progn (exchange-point-and-mark)
               (setq end (line-end-position)))
      (setq end (point-max)))
    (cons beg end)))

(defun better-editing/get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun better-editing/open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun better-editing/open-line-below ()
  "Insert an empty line below the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun better-editing//camel-to-underscore (keyword)
  "Convert KEYWORD from camel to underscore style.

like: userName -> user_name."
  (let (case-fold-search)
    (downcase
     (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1\_\\2" keyword))))

(defun better-editing//underscore-to-camel (keyword &optional upper)
  "Convert KEYWORD from underscore to camel style.

like: user_name -> userName.
Optional UPPER: user_name -> UserName."
  (let (result)
    (setq result (mapconcat
                  (lambda (s) (capitalize s))
                  (split-string keyword "_")
                  ""))
    (let ((first-char (substring result 0 1)))
      (concat
       (if upper (upcase first-char) (downcase first-char))
       (substring result 1)))))

(defun better-editing//region-or-symbol ()
  "Get the region or symbol under the cursor."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun better-editing/camel-to-underscore ()
  "Convert region or current symbol from camel to underscore style."
  (interactive)
  (let ((word (better-editing//region-or-symbol)))
    (better-editing/replace-string (better-editing//camel-to-underscore word))))

(defun better-editing/underscore-to-camel ()
  "Convert region or current symbol from underscore to camel style."
  (interactive)
  (let ((word (better-editing//region-or-symbol)))
    (better-editing/replace-string (better-editing//underscore-to-camel word))))

;; automatically save buffers associated with files when focus changed
(defun better-editing/auto-save ()
  "Save the current buffer."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defmacro better-editing/defadvice-commands (advice-name class commands &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar
        (lambda (command)
          `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
             ,@body))
        commands)))

;; multiple cursors functions
(defun mc//number-read-from-minibuffer ()
  "Read a number from the minibuffer."
  (require 'number)
  (number-read (read-from-minibuffer "Number: " "1")))

(defun mc/number/add (n)
  "Multiply-cursors support for number/add, N default 1."
  (interactive (list (mc//number-read-from-minibuffer)))
  (let* ((command (lambda () (interactive) (number/add n))))
    (mc/execute-command-for-all-cursors command)))

(defmacro better-editing/defun-mc-number-commands (commands)
  "Create mc number COMMANDS."
  `(progn
     ,@(mapcar
        (lambda (command)
          `(defun ,(intern (concat "mc/" (symbol-name command))) (n)
             ,(concat "Multiply-cursors support for " (symbol-name command) ", N default 1.")
             (interactive (list (mc//number-read-from-minibuffer)))
             (let* ((cmd (lambda () (interactive) (,command n))))
               (mc/execute-command-for-all-cursors cmd))))
        commands)))

(better-editing/defun-mc-number-commands (number/add number/divide number/multiply))

(defun mc/camel-to-underscore ()
  "Multiply-cursors support for camel-to-underscore."
  (interactive)
  (mc/execute-command-for-all-cursors 'better-editing/camel-to-underscore))

(defun mc/underscore-to-camel ()
  "Multiply-cursors support for underscore-to-camel."
  (interactive)
  (mc/execute-command-for-all-cursors 'better-editing/underscore-to-camel))

(defun better-editing/click-jump (event)
  "Ctrl + Mouse1 to jump to the definition or reference."
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((position (event-end event)))
    (if (not (windowp (posn-window position)))
        (error "Position not in text area of window"))
    (select-window (posn-window position))
    (let ((pt (posn-point position)))
      (when (numberp pt)
          (goto-char pt)
          (call-interactively 'spacemacs/helm-project-do-ag-region-or-symbol)
        ))))
