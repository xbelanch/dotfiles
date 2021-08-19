 ;;; -*- lexical-binding: t; -*-

;;; This is me!
(setq user-full-name "Xavier Belanche Alonso"
      user-mail-address "xbelanch@protonmail.com"
      calendar-latitude 41.499959
      calendar-longitude 2.181137
      calendar-location-name "Barcelona, Spain")

;;; Defer garbage collection
(setq gc-cons-threshold 100000000)

;;;
;;; Package Manager
;;; ===============
;;;
;;; Stolen from the @tsoding Package Manager
;;; from: https://github.com/rexim/dotfiles/blob/master/.emacs.rc/rc.el

(package-initialize)

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

;;; Compile buffer to show in a vertical buffer
;;; From: https://stackoverflow.com/questions/4157147/compile-buffer-to-show-in-a-vertical-buffer
(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    ad-do-it))

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

;;; Set font size to minibuffer
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
          '((default :height 1.25))))

;;; You can use M-y after C-y to insert previous item from the kill ring, or use browse-kill-ring package.
(delete-region (point) (line-end-position))
;;;
;;; Emacs Helpers
;;; ===================
;;;

;;; Grabbed from: https://emacs.stackexchange.com/questions/10348/revert-buffer-discard-unsaved-changes-without-y-n-prompt
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

;;; Mwim
(tsoding/require 'mwim)

;;; Avy
(tsoding/require 'avy)

;;; Anzu Query Replace
(tsoding/require 'anzu)

;;; Multiple cursors
(tsoding/require 'multiple-cursors)

;;; Move Text
(tsoding/require 'move-text)

;;; Ido everywhere
(tsoding/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

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

;;; CC-Mode
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))
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
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c C-r") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "C-c m") 'last-woman-standing)
(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)
(global-set-key [f5] 'revert-buffer-no-confirm)
(fset 'yes-or-no-p 'y-or-n-p)

;;;
;;; Custom Scratch Message
;;; ======================
;;;

(defun my-scratch-message ()
  (setq initial-scratch-message  ";;|-----------|\n;;| This      |\n;;| is        |\n;;| not       |\n;;| a         |\n;;| Scratch   |\n;;|-----------|\n;;(\\__/) ||\n;;(•ㅅ•) ||\n;;/ 　 づ\n\n"))
(add-hook 'after-init-hook #'my-scratch-message t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-space ((t (:foreground "gray75" :background))))
 '(whitespace-trailing ((t (:foreground "red" :background "yellow")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(org-agenda-files '("/mnt/f/IOC/Batxillerat/Materies/FA1B1/todo.org"))
 '(package-selected-packages
   '(ace-window ag anzu company csharp-mode dash-functional emmet-mode js2 gfm-mode glsl-mode graphviz-dot-mode helm helm-ag helm-cmd-t helm-git-grep helm-ls-git ido-completing-read+ js2-mode lua-mode magit markdown-mode move-text multiple-cursors mwim nasm-mode olivetti olivetti-mode rainbow-delimiters smartparens smex solarized-theme typescript-mode yaml-mode yasnippet zenburn-theme)))
