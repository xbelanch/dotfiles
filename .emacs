
;; Memory management
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)
;; Use UTF-8 by default. I don’t see why utf-8 should not be used everywhere as most systems support it.
(prefer-coding-system        'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; If you type part of keybind, Emacs will display this part in the echo area after a timeout. One second is a bit too long though for my taste.
(setq echo-keystrokes 0.1)

;; Save clipboard data of other programs in the kill ring when possible
(setq save-interprogram-paste-before-kill t)

;; Package management
;; https://emacs.stackexchange.com/questions/18030/function-naming-with-forward-slash
(defun xba/require-package (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package))
  (require package))

;; A modern list API for Emacs. No 'cl required.
(xba/require-package 'dash)
(xba/require-package 'dash-functional)

;; @TODO: Explain this
(require 'ansi-color)

;; @TODO: Explain this
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "C-c i m") 'imenu)

(setq-default inhibit-splash-screen t
              make-backup-files nil
              tab-width 4
              indent-tabs-mode nil
              compilation-scroll-output t
              visible-bell (equal system-type 'windows-nt))

;; Stolen from @tsoding's dotfiles (https://github.com/rexim/dotfiles/blob/a529f79ffe3bac19fe1ce842c3296ad792757df7/.emacs.rc/misc-rc.el#L14)
(defun rc/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'rc/colorize-compilation-buffer)

;; Stolen from @tsoding's dotfiles (https://github.com/rexim/dotfiles/blob/a529f79ffe3bac19fe1ce842c3296ad792757df7/.emacs.rc/misc-rc.el#L120)
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

(global-set-key (kbd "C-,") 'rc/duplicate-line)

;; Asking for confirmation when closing an unsaved file seems like a good idea. But entering ‘y’ or ‘n’ is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; Appareance
;; Zenburn Theme
(xba/require-package 'zenburn-theme)
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(font . "Iosevka-20"))
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(eval-after-load 'zenburn
  (set-face-attribute 'line-number nil :inherit 'default))
;;; Show buffer filename on title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; Ace-window manager
(xba/require-package 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)

;;; ido
(xba/require-package 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

<<<<<<< ours
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
||||||| base
;;; Helm
(tsoding/require 'helm 'helm-cmd-t 'helm-git-grep 'helm-ls-git 'helm-ag)
(setq helm-ff-transformer-show-only-basename nil)
(setq helm-ag-base-command "ag --nocolor --ignore-case --literal --line-number --column --stats --hidden --nogroup --ignore .git")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-insert-at-point 'symbol)
(setq helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'"))

;;; Dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer nil)

;;; Custom function for duplicate files on Dired
;;; Stolen from https://emacs.stackexchange.com/questions/60661/how-to-duplicate-a-file-in-dired
(defun dired-duplicate-this-file ()
  "Duplicate file on this line."
  (interactive)
  (let* ((this  (dired-get-filename t))
         (ctr   1)
         (new   (format "%s Copy" this)))
    (while (file-exists-p new)
      (setq ctr  (1+ ctr)
            new  (format "%s Copy (%d)" this ctr)))
     (dired-copy-file this new nil))
  (revert-buffer))

;;; Make man replace the same buffer
;;; Stolen from: https://emacs.stackexchange.com/questions/45174/how-to-make-man-replace-the-same-buffer
(defun last-woman-standing ()
  (interactive)
  (kill-matching-buffers "^\*WoMan .*\*" nil t)
  (call-interactively #'woman))

;;;
;;; Programming Helpers
;;; ===================
;;;

;;; compile buffer to show in a horizontal buffer
(setq split-width-threshold nil)
(setq split-height-threshold 0)

;;; Magit everywhere
(tsoding/require 'magit)
(setq magit-auto-revert-mode nil)

;;; Colorize compilation output
(require 'ansi-color)
(defun tsoding/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'tsoding/colorize-compilation-buffer)

;;; Smartparens
(tsoding/require 'smartparens)
(require 'smartparens)
(show-smartparens-global-mode +1)
(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)

;;; Company
(tsoding/require 'company)
(require 'company)
(require 'company-c-headers)
(global-company-mode)

;;; Yasnippet
(tsoding/require 'yasnippet)
(require 'yasnippet)
(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))
(yas-global-mode 1)

;;;
;;; Programming and Markup Languages
;;; ================================
;;;
=======
;;; Helm
(tsoding/require 'helm 'helm-cmd-t 'helm-git-grep 'helm-ls-git 'helm-ag)
(setq helm-ff-transformer-show-only-basename nil)
(setq helm-ag-base-command "ag --nocolor --ignore-case --literal --line-number --column --stats --hidden --nogroup --ignore .git")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-insert-at-point 'symbol)
(setq helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'"))

;;; Dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer nil)

;;; Custom function for duplicate files on Dired
;;; Stolen from https://emacs.stackexchange.com/questions/60661/how-to-duplicate-a-file-in-dired
(defun dired-duplicate-this-file ()
  "Duplicate file on this line."
  (interactive)
  (let* ((this  (dired-get-filename t))
         (ctr   1)
         (new   (format "%s Copy" this)))
    (while (file-exists-p new)
      (setq ctr  (1+ ctr)
            new  (format "%s Copy (%d)" this ctr)))
     (dired-copy-file this new nil))
  (revert-buffer))

;;; Make man replace the same buffer
;;; Stolen from: https://emacs.stackexchange.com/questions/45174/how-to-make-man-replace-the-same-buffer
(defun last-woman-standing ()
  (interactive)
  (kill-matching-buffers "^\*WoMan .*\*" nil t)
  (call-interactively #'woman))

;;;
;;; Programming Helpers
;;; ===================
;;;

;;; compile buffer to show in a horizontal buffer
(setq split-width-threshold nil)
(setq split-height-threshold 0)

;;; Magit everywhere
(tsoding/require 'magit)
(setq magit-auto-revert-mode nil)

;;; Colorize compilation output
(require 'ansi-color)
(defun tsoding/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'tsoding/colorize-compilation-buffer)

;;; Smartparens
(tsoding/require 'smartparens)
(require 'smartparens)
(show-smartparens-global-mode +1)
(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)

;;; Company
(tsoding/require 'company)
(tsoding/require 'company-c-headers)
(require 'company)
(require 'company-c-headers)
(global-company-mode)

;;; Yasnippet
(tsoding/require 'yasnippet)
(require 'yasnippet)
(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))
(yas-global-mode 1)

;;;
;;; Programming and Markup Languages
;;; ================================
;;;
>>>>>>> theirs

;;; c-mode
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

<<<<<<< ours
;;; Whitespace mode
;;; Stolen from https://github.com/rexim/dotfiles/blob/master/.emacs
;;; @TODO Display whitespace when mark is set
(xba/require-package 'whitespace)
(defun rc/set-up-whitespace-handling ()
||||||| base
;;; Makefile
(setq compile-command "./build.sh")

;;; GDB
(setq gdb-many-windows t
      gdb-show-main t)

;;; Markdown/GFM
(tsoding/require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; Packages that don't require configuration
(tsoding/require
 'js2-mode
 'emmet-mode
 'nasm-mode
 'yaml-mode
 'glsl-mode
 'lua-mode
 'graphviz-dot-mode
 'olivetti
 'typescript-mode
 )

(add-to-list 'load-path "~/.emacs.local/")
(require 'arma-mode)

;;;
;;; Org: Fast access to TODO states
;;; ===============================
;;;

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

;;;
;;; Default global values
;;; =====================
;;;

(setq-default inhibit-splash-screen t
              inhibit-startup-message t
              debug-on-error nil
              make-backup-files nil
              message-log-max 500
              tab-width 4
              warning-suppress-types nil
              indent-tabs-mode nil
              display-time-default-load-average nil
              cursor-in-non-selected-windows t
              compilation-scroll-output t
              fill-column 80 ;; Set width for automatic line breaks
              vc-follow-symlinks t
              electric-pair-preserve-balance nil
              global-auto-revert-mode t
              auto-save-interval  2048
              indicate-empty-lines t
              indent-tabs-mode nil
              display-time-mode 1
              show-paren-mode 1
              mode-require-final-newline nil
              visible-bell nil) ;; No flashing, please.

;;;
;;; Custom functions
;;; ================
;;;

;;; Stolen again from @tsoding:
;;; https://github.com/rexim/dotfiles/blob/master/.emacs.tsoding/misc-rc.el#L120
(defun tsoding/duplicate-line ()
  "Duplicate current line"
=======
;;; Makefile
(setq compile-command "./build.sh")

;;; GDB
(setq gdb-many-windows t
      gdb-show-main t)

;;; Markdown/GFM
(tsoding/require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; Packages that don't require configuration
(tsoding/require
 'js2-mode
 'emmet-mode
 'nasm-mode
 'yaml-mode
 'glsl-mode
 'lua-mode
 'graphviz-dot-mode
 'olivetti
 'typescript-mode
 'go-mode
 )

(add-to-list 'load-path "~/.emacs.local/")
(require 'arma-mode)

;;;
;;; Org: Fast access to TODO states
;;; ===============================
;;;

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

;;;
;;; Default global values
;;; =====================
;;;

(setq-default inhibit-splash-screen t
              inhibit-startup-message t
              debug-on-error nil
              make-backup-files nil
              message-log-max 500
              tab-width 4
              warning-suppress-types nil
              indent-tabs-mode nil
              display-time-default-load-average nil
              cursor-in-non-selected-windows t
              compilation-scroll-output t
              fill-column 80 ;; Set width for automatic line breaks
              vc-follow-symlinks t
              electric-pair-preserve-balance nil
              global-auto-revert-mode t
              auto-save-interval  2048
              indicate-empty-lines t
              indent-tabs-mode nil
              display-time-mode 1
              show-paren-mode 1
              mode-require-final-newline nil
              visible-bell nil) ;; No flashing, please.

;;;
;;; Custom functions
;;; ================
;;;

;;; Stolen again from @tsoding:
;;; https://github.com/rexim/dotfiles/blob/master/.emacs.tsoding/misc-rc.el#L120
(defun tsoding/duplicate-line ()
  "Duplicate current line"
>>>>>>> theirs
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'asm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'nasm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)

;;; magit
;; magit requres this lib, but it is not installed automatically on
;; Windows.
(xba/require-package 'magit)
(setq magit-auto-revert-mode nil)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;;; helm
(xba/require-package 'helm)

;;; Company
(xba/require-package 'company)
(global-company-mode)

;;; Custom keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-q") 'kill-this-buffer)

;;; Display line numbers
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
<<<<<<< ours
   '(paredit zenburn-theme yasnippet yaml-mode typescript-mode smex smartparens rainbow-delimiters olivetti nasm-mode mwim multiple-cursors move-text markdown-mode magit lua-mode js2-mode ido-completing-read+ helm-ls-git helm-git-grep helm-cmd-t helm-ag gruber-darker-theme graphviz-dot-mode glsl-mode emmet-mode dash-functional company-c-headers anzu ace-window))
 '(display-line-numbers-type (quote relative))
 '(whitespace-style (quote (face tabs trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
||||||| base
   '(company-c-headers gruber-darker-theme ace-window ag anzu company csharp-mode dash-functional emmet-mode js2 gfm-mode glsl-mode graphviz-dot-mode helm helm-ag helm-cmd-t helm-git-grep helm-ls-git ido-completing-read+ js2-mode lua-mode magit markdown-mode move-text multiple-cursors mwim nasm-mode olivetti olivetti-mode rainbow-delimiters smartparens smex solarized-theme typescript-mode yaml-mode yasnippet zenburn-theme)))
(put 'downcase-region 'disabled nil)
=======
   '(company-c-headers gruber-darker-theme ace-window ag anzu company csharp-mode dash-functional emmet-mode js2 gfm-mode glsl-mode graphviz-dot-mode helm helm-ag helm-cmd-t helm-git-grep helm-ls-git ido-completing-read+ js2-mode lua-mode magit markdown-mode move-text multiple-cursors mwim nasm-mode olivetti olivetti-mode rainbow-delimiters smartparens smex solarized-theme typescript-mode yaml-mode yasnippet zenburn-theme)))
(put 'downcase-region 'disabled nil)
>>>>>>> theirs
