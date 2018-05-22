                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; Fridgeton's custom emacs init settings ;;
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Package handling
;; ----------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'whitespace)
(require 'vdiff)
(require 'custom-lisp-functions)

;; Default settings
;; ----------------
;; Set default theme
(load-theme 'paganini t)
(show-trailing-whitespace)
;; General default settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq-default column-number-mode t)
(setq-default vdiff-auto-refine t)
;; Activate vdiff shortcuts
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
;; C/C++ indentation and code style
(c-add-style "custom-c-code-style"
             '("stroustrup" ;; Inherit from Stroustrup indentation style
               (c-basic-offset  . 4)
               (c-offsets-alist . ((innamespace           . 0) ;; Don't indent namespaces
                                   (substatement          . +) ;; To make namespace indentation work
                                   (inclass               . +) ;; To make namespace indentation work
                                   (arglist-cont-nonempty . +)
                                   (statement-cont        . (add c-lineup-match))
                                   ))))
(setq-default c-default-style "custom-c-code-style")
;; Associate .inl files with c++ mode
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
;; Add relative indentation (line-up) with C-tab
(global-set-key (kbd "C-<tab>") 'indent-relative)
;; Add backtab (Shift + tab) as the custom function "remove tab"
(global-set-key (kbd "<backtab>") 'remove-tab)
(global-set-key (kbd "M-g C-g") 'goto-column)
;; Bind M-j to join next line with current one
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))


; TODO
;; Major mode hooks
;; ================
;; C/C++ mode
;; ----------
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "custom-c-code-style")
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            (show-trailing-whitespace)
            (setq fill-column 80)
            (setq column-number-mode t)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (linum-mode 1)
            (fci-mode 1)
            ))

;; CMake mode
;; ----------
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            (linum-mode 1)
            (fci-mode 0)
            ))

;; Emacs lisp mode
;; ---------------
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (linum-mode 1)
            (fci-mode 0)
            (show-trailing-whitespace)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            ))

;; Python mode
;; -----------
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (linum-mode 1)
            (fci-mode 1)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            ))

;; TODO add mode hooks

;; Other hooks
;; ===========
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
;; Don't automatically enable electric indent mode
;; (add-hook 'after-change-major-mode-hook
;;           (lambda()
;;             (electric-indent-mode -1)))



;; Automatically generated code telling emacs to "trust" certain themes
;; |-->
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" "65d9573b64ec94844f95e6055fe7a82451215f551c45275ca5b78653d505bc42" "2b6bd2ebad907ee42b3ffefa4831f348e3652ea8245570cdda67f0034f07db93" "7f3ef7724515515443f961ef87fee655750512473b1f5bf890e2dc7e065f240c" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" default)))
 '(package-selected-packages
   (quote
    (company-irony flycheck-irony irony monokai-theme paganini-theme vdiff-magit vdiff color-theme fill-column-indicator hemisu-theme gruvbox-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; -->| End of auto generated code
