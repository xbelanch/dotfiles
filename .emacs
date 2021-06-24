;;; -*- lexical-binding: t; -*-

;;; This is me!
(setq user-full-name "Xavier Belanche Alonso"
      user-mail-address "xbelanch@protonmail.com"
      calendar-latitude 41.499959
      calendar-longitude 2.181137
      calendar-location-name "Barcelona, Spain")

;;; Defer garbage collection
(setq gc-cons-threshold 100000000)


(package-initialize)

;;;
;;; Package Manager
;;; ===============
;;;
;;; Stolen from the @tsoding Package Manager
;;; from: https://github.com/rexim/dotfiles/blob/master/.emacs.rc/rc.el

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defvar tsoding/package-contents-refreshed nil)

(defun tsoding/package-refresh-contents-once ()
  (when (not tsoding/package-contents-refreshed)
    (setq tsoding/package-contents-refreshed t)
    (package-refresh-contents)))

(defun tsoding/require-one-package (package)
  (when (not (package-installed-p package))
    (tsoding/package-refresh-contents-once)
    (package-install package)))

(defun tsoding/require (&rest packages)
  (dolist (package packages)
    (tsoding/require-one-package package)))

;;; TODO: Need to give support form theme variants (darker, light...)
(defun tsoding/require-theme (theme)
  (let ((theme-package (->> theme
                            (symbol-name)
                            (funcall (-flip #'concat) "-theme")
                            (intern))))
    (tsoding/require theme-package)))

;;; We need dash modern list librart at this point
(tsoding/require 'dash)
(require 'dash)

(tsoding/require 'dash-functional)
(require 'dash-functional)

;;; Choose a monospace font always is hard to decide
(defun tsoding/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) "Ubuntu Mono-18")))

(add-to-list 'default-frame-alist `(font . ,(tsoding/get-default-font)))

;;;
;;; GUI / Window / Theme
;;; ====================
;;;

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;;; Show buffer filename on title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; Set Zenburn Theme
(tsoding/require-theme 'zenburn)
(load-theme 'zenburn t)

;;; Ace-window manager
(tsoding/require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)

;;; Display relative line numbers
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; Visual goodie for delimiters
(tsoding/require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; Display white spaces and white trailing spaces
(tsoding/require 'whitespace)
(whitespace-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))

;;; Colorful man
;;; stolen from: https://emacs.stackexchange.com/questions/14245/is-there-a-way-to-view-the-man-pages-in-color-in-emacs
(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

;;; Set the cursor as a box
(set-cursor-color "#ffff00")

;;;
;;; Emacs Helpers
;;; ===================
;;;

;;; Ido everywhere
(tsoding/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;;; Helm
(tsoding/require 'helm 'helm-cmd-t 'helm-git-grep 'helm-ls-git)
(setq helm-ff-transformer-show-only-basename nil)


;;;
;;; Programming Helpers
;;; ===================
;;;

;;; Smartparens
(tsoding/require 'smartparens)
(require 'smartparens)
(show-smartparens-global-mode +1)
(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)

;;; Company
(tsoding/require 'company)
(require 'company)
(global-company-mode)

;;; Yasnippet
(tsoding/require 'yasnippet)
(require 'yasnippet)
(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))
(yas-global-mode 1)

;;;
;;; Default global values
;;; =====================
;;;
(require 'ansi-color)

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
              column-number-mode 1
              display-time-mode 1
              show-paren-mode 1
              visible-bell nil) ;; No flashing, please.

;;;
;;; Custom functions
;;; ================
;;;

;;; Stolen again from @tsoding:
;;; https://github.com/rexim/dotfiles/blob/master/.emacs.tsoding/misc-rc.el#L120
(defun tsoding/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

;;;
;;; Keybindings
;;; ===========
;;;

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-q") 'kill-this-buffer)
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "C-c i m") 'imenu)
(global-set-key (kbd "C-,") 'tsoding/duplicate-line)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "C-c h g g") 'helm-git-grep)
(global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h r") 'helm-recentf)

(fset 'yes-or-no-p 'y-or-n-p)

;;;
;;; Custom Scratch Message
;;; ======================
;;;

(defun my-scratch-message ()
  (setq initial-scratch-message  ";;|-----------|\n;;| This      |\n;;| is        |\n;;| not       |\n;;| a         |\n;;| Scratch   |\n;;|-----------|\n;;(\\__/) ||\n;;(•ㅅ•) ||\n;;/ 　 づ\n\n"))
(add-hook 'after-init-hook #'my-scratch-message t)



;; ;;; Some helpers





;; (setq-default display-line-numbers-type (quote relative))


;; ;;; Whitespace style
;; (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ;;; Set Default values
;; (setq-default inhibit-splash-screen t
;;               make-backup-files nil
;;               tab-width 4
;;               indent-tabs-mode nil
;;               compilation-scroll-output t
;;               compilation-environment '("LANG=C")
;;               visible-bell (equal system-type 'windows-nt))



;; ;;; Change all prompts to y or n
;; (fset 'yes-or-no-p 'y-or-n-p)

;; ;;; Duplicate lines
;; (defun tsoding/duplicate-line ()
;;   "Duplicate current line"
;;   (interactive)
;;   (move-beginning-of-line 1)
;;   (kill-line)
;;   (yank)
;;   (newline)
;;   (yank))
;; (global-set-key (kbd "C-,") 'tsoding/duplicate-line)

;; ;;; Default Encoding
;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (if (eq system-type 'windows-nt)
;;     (set-clipboard-coding-system 'utf-16le-dos)
;;   (set-clipboard-coding-system 'utf-8))


;; ;;; Anzu
;; (tsoding/require 'anzu)
;; (global-anzu-mode 1)
;; (global-set-key (kbd "C-c r") 'anzu-query-replace-regexp)

;; ;;; Colorize compilation output
;; (require 'ansi-color)
;; (defun tsoding/colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region compilation-filter-start (point))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'tsoding/colorize-compilation-buffer)

;; ;;; Whitespace mode
;; (defun tsoding/set-up-whitespace-handling ()
;;   (interactive)
;;   (whitespace-mode 1)
;;   (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;; (add-hook 'c-mode-hook 'tsoding/set-up-whitespace-handling)
;; (add-hook 'c++-mode-hook 'tsoding/set-up-whitespace-handling)
;; (add-hook 'markdown-mode-hook 'tsoding/set-up-whitespace-handling)
;; (add-hook 'yaml-mode-hook 'tsoding/set-up-whitespace-handling)

;; ;;; c-mode
;; (setq-default c-basic-offset 4
;;               c-default-style '((java-mode . "java")
;;                                 (awk-mode . "awk")
;;                                 (other . "bsd")))
;; (add-hook 'c-mode-hook (lambda ()
;;                          (interactive)
;;                          (c-toggle-comment-style -1)))

;; ;;; Magit everywhere
;; (tsoding/require 'magit)
;; (setq magit-auto-revert-mode nil)

;; ;;; dired
;; (require 'dired-x)
;; (setq dired-omit-files
;;       (concat dired-omit-files "\\|^\\..+$"))
;; (setq-default dired-dwim-target t)
;; (setq dired-listing-switches "-alh")

;; (defun dired-duplicate-this-file ()
;;   "Duplicate file on this line"
;;   (interactive)
;;   (let* ((this (dired-get-filename t))
;;          (ctr 1)
;;          (new (format "%s[%d]" this ctr)))
;;     (while (file-exists-p new)
;;       (setq ctr (1+ ctr)
;;             new (format "%s[%d]" this ctr)))
;;     (dired-copy-file this new nil))
;;   (revert-buffer))


;; ;;; helm
;; (tsoding/require 'helm)
;; (setq helm-ff-transformer-show-only-basename nil)
;; (global-set-key (kbd "C-c h f") 'helm-find)
;; (global-set-key (kbd "C-c h r") 'helm-recentf)


;; ;;; word-wrap
;; (defun tsoding/enable-word-wrap ()
;;   (interactive)
;;   (toggle-word-wrap 1))
;; (add-hook 'markdown-mode-hook 'tsoding/enable-word-wrap)


;; ;;; Move Text
;; (tsoding/require 'move-text)
;; (global-set-key (kbd "M-p") 'move-text-up)
;; (global-set-key (kbd "M-n") 'move-text-down)

;; ;;; Packages that don't require configuration
;; (tsoding/require
;;  'yaml-mode
;;  'glsl-mode
;;  'lua-mode
;;  'graphviz-dot-mode
;;  'markdown-mode
;;  'olivetti
;;  'ag
;;  'typescript-mode
;;  )

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(blink-cursor-blinks 0)
;;  '(custom-safe-themes
;;    '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
;;  '(electric-pair-mode t)
;;  '(package-selected-packages '(zenburn-theme)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(simple rainbow-delimiters ace-window ag anzu company dash-functional glsl-mode graphviz-dot-mode helm ido-completing-read+ lua-mode magit markdown-mode move-text olivetti smex solarized-theme typescript-mode yaml-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-space ((t (:bold t :foreground "gray75" :background))))
 '(whitespace-trailing ((t (:foreground "red" :background "yellow")))))
