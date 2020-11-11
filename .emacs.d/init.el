
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; simnil's emacs config ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Package handling
;; ----------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'cmake-mode)
(require 'company)
(require 'linum)
(require 'org)
(require 'org-bullets)
(require 'powerline)
(require 'python)
(require 'rtags)
(require 'rust-mode)
(require 'server)
(require 'vdiff)
(require 'whitespace)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'custom-lisp-functions)


;; Default settings
;; ----------------
(setq initial-frame-alist '((width  . 85)
                            (height . 50)))
(setq default-frame-alist '((width  . 85)
                            (height . 50)))
(load-theme 'paganini t)
(powerline-default-theme)
(visual-line-mode 1)
(show-trailing-whitespace)
(global-linum-mode 1)
(tool-bar-mode -1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 95)
(setq-default column-number-mode t)
(setq-default vdiff-auto-refine t)
(setq-default ring-bell-function 'ignore)
(setq-default make-backup-files nil)
(setq-default company-idle-delay 0)
(setq-default company-minimum-prefix-length 2)
(add-to-list 'company-backends 'company-jedi) ; Python auto-completion
;; Use Dejavu Sans Mono if default font cannot display char
(set-fontset-font "fontset-default" nil (font-spec :name "Dejavu Sans Mono")
                  nil 'append)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map) ; Activate vdiff shortcuts
(rtags-enable-standard-keybindings)

;; C/C++ indentation and code style
(c-add-style "custom-code-style"
             '("stroustrup" ; Inherit from Stroustrup indentation style
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
(setq-default c-default-style "custom-line-up-args")
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode)) ; Associate .inl files with c++ mode


;; Org mode
;; --------
(org-babel-do-load-languages 'org-babel-load-languages
                             '((python . t)
                               (shell  . t) ; includes bash
                               (C      . t) ; includes C++ and D
                               ))
(setq-default org-babel-python-command "python3")
(setq-default org-confirm-babel-evaluate nil)
;; Use Dejavu Sans Mono for these specific ranges of unicode points
(set-fontset-font "fontset-default" '(#x25A0 . #x25FF) (font-spec :name "Dejavu Sans Mono"))
(setq-default org-bullets-bullet-list '("◉" "○" "◈" "◇")) ; U+25C9, U+25CB, U+25C6, U+25C7
;; For some of the default bullets in org-bullets that aren't in the range above
(set-fontset-font "fontset-default" '(#x273F . #x2740) (font-spec :name "Dejavu Sans Mono"))
;; Fancier TODOs, but currently not used since they're more difficult to search and filter
;; (setq-default org-todo-keywords
;;               '((sequence "▢ TODO" "|" "✔ DONE" "✘ CANCELLED"))) ; U+25A2, U+2714, U+2718
(setq-default org-directory "~/org")
(setq-default shared-org-directory "~/Dropbox/orgfiles")
(setq-default org-agenda-files (list org-directory shared-org-directory))
(setq-default org-default-notes-file (concat org-directory "/notes.org"))
(setq-default org-capture-templates
              '(("t" "Task" entry
                 (file+headline (lambda () (concat shared-org-directory "/tasks.org")) "Tasks")
                 "* TODO %? %^G\n  %i"
                 :empty-lines 1)
                ("n" "Note" entry
                 (file+headline (lambda () (concat shared-org-directory "/notes.org")) "Notes")
                 "* %? %^G\n  Taken on: %u\n  %i"
                 :prepend t :empty-lines 1)
                ("j" "Journal" entry
                 (file+datetree (lambda () (concat shared-org-directory "/journal.org")))
                 "* %?\n  Entered on: %U\n  %i"
                 :empty-lines 1)))
(setq-default org-startup-indented t)
(setq-default org-tags-column -90)


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
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)


;; Major mode hooks
;; ----------------
(add-hook 'c++-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq fill-column 95)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (fci-mode 1)
            (flycheck-mode 1)
            ))
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq cmake-tab-width 2)
            (fci-mode 1)
            (flycheck-mode 1)
            ))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (fci-mode 1)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (flycheck-mode 1)
            ))
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (local-set-key (kbd "C-c C-<tab>") 'org-global-cycle)
            ))
(add-hook 'js-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq fill-column 95)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (fci-mode 1)
            (flycheck-mode 1)
            ))
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (fci-mode 1)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (flycheck-mode 1)
            ))
(add-hook 'rust-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq fill-column 95)
            (fci-mode 1)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (flycheck-mode 1)
            ))
(add-hook 'sh-mode-hook ; shell script mode
          (lambda ()
            (setq tab-width 4)
            (fci-mode 1)
            (electric-pair-mode 1)
            (show-paren-mode 1)
            (flycheck-mode 1)
            ))
(add-hook 'shell-mode-hook ; interactive shell mode
          (lambda() (add-hook
                     'comint-output-filter-functions
                     'python-pdbtrack-comint-output-filter-function t)))


;; Other hooks
;; -----------
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)


;; Start emacsclient server
;; To allow opening files in a running emacs session
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
      (server-start))


(cmake-ide-setup)


;; Automatically generated code |-->
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
    (org company-jedi cargo ob-rust rust-mode flycheck-rust org-bullets gnuplot gnuplot-mode glsl-mode markdown-mode markdown-mode+ markdown-preview-mode auctex flymake-lua lua-mode cmake-mode powerline cmake-ide company-rtags flycheck-rtags rtags company flycheck mustang-theme monokai-theme paganini-theme vdiff-magit vdiff color-theme fill-column-indicator hemisu-theme gruvbox-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; -->| End of auto generated code
