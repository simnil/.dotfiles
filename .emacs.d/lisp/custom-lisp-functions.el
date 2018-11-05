                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; Fridgeton's custom lisp functions ;;
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'custom-lisp-functions)


(defun fill-to-col (char col)
  "Inserts character 'char' forward up to column 'col'"
  (interactive "cChar: \nnColumn: ")
  (insert (make-string
           (max 0
                (- col (current-column)))
           char)))


(defun col-insert (char col)
  "Inserts character 'char' at column 'col'"
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
  "Replaces character at column 'col' with 'char'"
  (interactive "cChar: \nnColumn: ")
  (progn
    (col-insert char col)
    ;; Don't delete a character if at the end of the line
    (if (eq (- (line-end-position) (line-beginning-position))
            (current-column))
        nil
      (delete-char 1))))


(defun goto-column (col)
  "Places marker at column 'col' on current line. Appends spaces to end of line if needed"
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
  "Show trailing whitespace characters in buffer"
  (interactive)
  (progn
    (whitespace-mode 0)
    (setq whitespace-style '(face trailing))
    (whitespace-mode 1)))


(defun show-all-whitespace ()
  "Show all (or whatever is default for whitespace-style) whitespace characters"
  (interactive)
  (progn
    (whitespace-mode 0)
    (setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark ...))
    (whitespace-mode 1)))


(defun remove-tab ()
  "Removes one tab or 'tab-width' whitespaces from the current line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string tab-width ?\ )))
        (replace-match "")))))


;; Taken from whattheemacsd.com ("rotate-windows")
(defun swap-windows ()
  "Swaps buffers between windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't swap a single window!"))
        (t
         (setq i 1)
         (setq num-windows (count-windows))
         (while (< i num-windows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i num-windows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))


(defun underline-line ()
  "Inserts a line below the current one, underlining it with '-' characters"
  (interactive)
  (progn
    (end-of-line)
    (let ((line-length (current-column)))
      (if (string= major-mode
                   "c++-mode")
          (c-indent-new-comment-line)
        (indent-new-comment-line))
      (fill-to-col ?- line-length))))


;; Functions for conveniently switching theme
;; ------------------------------------------
(defun monokai ()
  (interactive)
  (load-theme 'monokai t))
(defun paganini ()
  (interactive)
  (load-theme 'paganini t))
(defun gruvbox ()
  (interactive)
  (load-theme 'gruvbox-dark-medium t))
(defun mustang ()
  (interactive)
  (load-theme 'mustang t))
;; ------------------------------------------
