;;; custom-lisp-functions.el --- Miscellaneous utility functions

;; Author: Simon Nilsson

;; Feel free to use any of this code in your own setup as if it was your own,
;; UNLESS I have provided a source for it (modified or not). Then you should
;; provide a source too, to not misrepresent someone else's work.

;;; Commentary:
;; This "package" contains various utility functions I use in my Emacs setup.
;; It's meant to be loaded from .emacs.d/init.el file (or similar) with
;; something like:
;;
;;   (add-to-list 'load-path "~/.emacs.d/lisp")
;;   (require 'custom-lisp-functions)

;;; Code:

(defun fill-to-col (char col)
  "Insert character 'CHAR' forward up to column 'COL'."
  (interactive "cChar: \nnColumn: ")
  (insert (make-string
           (max 0
                (- col (current-column)))
           char)))


(defun col-insert (char col)
  "Insert character 'CHAR' at column 'COL'."
  (interactive "cChar: \nnColumn: ")
  (if (> (current-column) col)
      (progn
        (beginning-of-line)
        (forward-char col)
        (insert char))
    (progn
      ;; If line ends before target column
      (if (< (- (line-end-position) (line-beginning-position))
             col)
          (progn
            (end-of-line)
            (fill-to-col ?\s col))
        (forward-char (- col (current-column)) ))
      (insert char))))


(defun col-replace (char col)
  "Replace character at column 'COL' with 'CHAR'."
  (interactive "cChar: \nnColumn: ")
  (progn
    (col-insert char col)
    ;; Don't delete a character if at the end of the line
    (if (eq (- (line-end-position) (line-beginning-position))
            (current-column))
        nil
      (delete-char 1))))


(defun goto-column (col)
  "Place marker at column 'COL' on current line.  Append spaces to end of line if needed."
  (interactive "nGoto column: ")
  (let ((line-length (- (line-end-position) (line-beginning-position))))
    (if (< col line-length)
        (progn
          (end-of-line)
          (fill-to-col ?\s col)))
    (progn
      (beginning-of-line)
      (forward-char col))))


(defun show-trailing-whitespace ()
  "Show trailing whitespace characters in buffer."
  (interactive)
  (progn
    (whitespace-mode 0)
    (setq whitespace-style '(face trailing))
    (whitespace-mode 1)))


(defun show-all-whitespace ()
  "Show all (or whatever is default for whitespace-style) whitespace characters."
  (interactive)
  (progn
    (whitespace-mode 0)
    (setq whitespace-style '(face tabs spaces trailing lines space-before-tab
                                  newline indentation empty space-after-tab
                                  space-mark tab-mark newline-mark))
    (whitespace-mode 1)))


(defun remove-tab ()
  "Remove one tab or 'TAB-WIDTH' whitespaces from the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string tab-width ?\ )))
        (replace-match "")))))


;; Originally taken from whattheemacsd.com ("rotate-windows").
;; The code has been modified.
(defun swap-windows ()
  "Swap buffers between windows."
  (interactive)
  (if (<= (count-windows) 1)
      (message "You can't swap a single window!")
    (let ((i 1)
          (num-windows (count-windows)))
      (while (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))

               (b1 (window-buffer w1))
               (b2 (window-buffer w2))

               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i)))))))


(defun underline-line (char)
  "Insert a line below the current one, underlining it with 'CHAR' characters."
  (interactive "cCharacter: ")
  (progn
    (end-of-line)
    (let ((line-length (current-column)))
      (if (string= major-mode
                   "c++-mode")
          (c-indent-new-comment-line)
        (indent-new-comment-line))
      (fill-to-col char line-length))))


;; Functions for conveniently switching theme
;; ------------------------------------------
(defun monokai ()
  "Switch to monokai theme."
  (interactive)
  (load-theme 'monokai t))
(defun paganini ()
  "Switch to paganini theme."
  (interactive)
  (load-theme 'paganini t))
(defun gruvbox ()
  "Switch to gruvbox theme."
  (interactive)
  (load-theme 'gruvbox-dark-medium t))
(defun mustang ()
  "Switch to mustang theme."
  (interactive)
  (load-theme 'mustang t))
;; ------------------------------------------

(provide 'custom-lisp-functions)
;;; custom-lisp-functions ends here
