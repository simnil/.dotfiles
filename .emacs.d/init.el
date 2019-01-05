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
(require 'whitespace)
(require 'linum)
(require 'vdiff)
(require 'python)
(require 'server)
(require 'rtags)
(require 'powerline)
(require 'cmake-mode)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'custom-lisp-functions)


;; Default settings
;; ----------------
;; Set default theme
(load-theme 'paganini t)
(powerline-default-theme)
(show-trailing-whitespace)
;; General default settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 95)
(setq-default column-number-mode t)
(setq-default vdiff-auto-refine t)
(setq-default ring-bell-function 'ignore)
(tool-bar-mode -1)
;; Activate vdiff shortcuts
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
;; Enable rtags shortcuts
(rtags-enable-standard-keybindings)
;; C/C++ indentation and code style
(c-add-style "custom-code-style"
             '("stroustrup" ;; Inherit from Stroustrup indentation style
               (c-basic-offset  . 4)
               (c-offsets-alist . ((innamespace           . 0)
                                   (inline-open           . 0)
                                   (arglist-cont-nonempty . +)
                                   (inher-cont            . c-lineup-multi-inher)
                                   (statement-cont        . (first c-lineup-math
                                                                   c-lineup-cascaded-calls
                                                                   +))
                                   ))))
(c-add-style "custom-line-up-args"
             '("custom-code-style"
               (c-offsets-alist . ((arglist-cont-nonempty . c-lineup-arglist)
                                   ))))
(setq-default c-default-style "custom-code-style")
;; Associate .inl files with c++ mode
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

;; Custom keybindings
;; ------------------
(global-set-key (kbd "C-<tab>") 'indent-relative)
(global-set-key (kbd "<backtab>") 'remove-tab)
(global-set-key (kbd "M-g C-g") 'goto-column)
(global-set-key (kbd "C-c u") (lambda () (interactive) (underline-line ?-)))
(global-set-key (kbd "C-c U") (lambda () (interactive) (underline-line ?=)))
;; Bind M-j to join next line with current one
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(define-key c++-mode-map (kbd "M-j") nil)


;; Major mode hooks
;; ================

;; C/C++ mode
;; ----------
(add-hook 'c++-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            (show-trailing-whitespace)
            (setq fill-column 95)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (linum-mode 1)
            (fci-mode 1)
            (flycheck-mode 1)
            ))

;; CMake mode
;; ----------
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq cmake-tab-width 4)
            (setq indent-tabs-mode nil)
            (linum-mode 1)
            (fci-mode 0)
            (flycheck-mode 1)
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
            (flycheck-mode 1)
            ))

;; Javascript mode
;; ---------------
(add-hook 'js-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            (show-trailing-whitespace)
            (setq fill-column 95)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (linum-mode 1)
            (fci-mode 1)
            (flycheck-mode 1)
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
            (flycheck-mode 1)
            ))

;; Shell script mode
;; -----------------
(add-hook 'sh-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (show-trailing-whitespace)
            (linum-mode 1)
            (fci-mode 1)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (flycheck-mode 1)
            ))

;; Shell mode
;; ----------
(add-hook 'shell-mode-hook
          (lambda() (add-hook
                     'comint-output-filter-functions
                     'python-pdbtrack-comint-output-filter-function t)))


;; Other hooks
;; ===========
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
;; Don't automatically enable electric indent mode
;; (add-hook 'after-change-major-mode-hook
;;           (lambda()
;;             (electric-indent-mode -1)))


;; Start emacsclient server
;; To allow opening files in a running emacs session
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
      (server-start))


;; Start cmake-ide
(cmake-ide-setup)


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
    (flymake-lua lua-mode cmake-mode powerline cmake-ide company-rtags flycheck-rtags rtags company flycheck mustang-theme monokai-theme paganini-theme vdiff-magit vdiff color-theme fill-column-indicator hemisu-theme gruvbox-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; -->| End of auto generated code
