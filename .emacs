(package-initialize)

;;; Tsoding Package manager
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

;; TODO: Need to give support form theme variants (darker, light...)
(defun rc/require-theme (theme)
  (let ((theme-package (->> theme
                            (symbol-name)
                            (funcall (-flip #'concat) "-theme")
                            (intern))))
    (rc/require theme-package)))

;;;; We need dash modern list librart at this point
(rc/require 'dash)
(require 'dash)

(rc/require 'dash-functional)
(require 'dash-functional)

;;; Appearance
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) "Ubuntu Mono-16")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

;;; GUI / Window
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

;;; Useful keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-q") 'kill-this-buffer)

;;; Display line numbers
;;; copypasta from https://www.reddit.com/r/emacs/comments/99e49n/displaylinenumbersmode_relative_line_numbers/
(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 0
              display-line-numbers-widen t)
;; (add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; Whitespace style
(setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Set Default values
(setq-default inhibit-splash-screen t
              make-backup-files nil
              tab-width 4
              indent-tabs-mode nil
              compilation-scroll-output t
              compilation-environment '("LANG=C")
              visible-bell (equal system-type 'windows-nt))

;;; Set Theme
(rc/require-theme 'solarized)
;;; TODO: This must be included on require-theme funtion
(load-theme 'solarized-dark t)

;;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;; Duplicate lines
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))
(global-set-key (kbd "C-,") 'rc/duplicate-line)

;;; Default Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(if (eq system-type 'windows-nt)
    (set-clipboard-coding-system 'utf-16le-dos)
  (set-clipboard-coding-system 'utf-8))

;;; Ido
(rc/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(global-set-key (kbd "M-x") 'smex)

;;; Anzu
(rc/require 'anzu)
(global-anzu-mode 1)
(global-set-key (kbd "C-c r") 'anzu-query-replace-regexp)

;;; Colorize compilation output
(require 'ansi-color)
(defun rc/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'rc/colorize-compilation-buffer)

;;; Whitespace mode
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)

;;; c-mode
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

;;; Magit everywhere
(rc/require 'magit)
(setq magit-auto-revert-mode nil)

;;; dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

(defun dired-duplicate-this-file ()
  "Duplicate file on this line"
  (interactive)
  (let* ((this (dired-get-filename t))
         (ctr 1)
         (new (format "%s[%d]" this ctr)))
    (while (file-exists-p new)
      (setq ctr (1+ ctr)
            new (format "%s[%d]" this ctr)))
    (dired-copy-file this new nil))
  (revert-buffer))


;;; helm
(rc/require 'helm)
(setq helm-ff-transformer-show-only-basename nil)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h r") 'helm-recentf)

;;; yasnippet
(rc/require 'yasnippet)
(require 'yasnippet)
(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))
(yas-global-mode 1)

;;; word-wrap
(defun rc/enable-word-wrap ()
  (interactive)
  (toggle-word-wrap 1))
(add-hook 'markdown-mode-hook 'rc/enable-word-wrap)

;;; Company
(rc/require 'company)
(require 'company)
(global-company-mode)

;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; Packages that don't require configuration
(rc/require
 'yaml-mode
 'glsl-mode
 'lua-mode
 'graphviz-dot-mode
 'markdown-mode
 'olivetti
 'ag
 'typescript-mode
 )

;;; Custom scratch message
(defun my-scratch-message ()
  (setq initial-scratch-message  ";;|-----------|\n;;| This      |\n;;| is        |\n;;| not       |\n;;| a         |\n;;| Scratch   |\n;;|-----------|\n;;(\\__/) ||\n;;(•ㅅ•) ||\n;;/ 　 づ\n\n"))
(add-hook 'after-init-hook #'my-scratch-message t)

;;; Set the cursor as a box
(set-cursor-color "#ffff00")

;;; Ace-window manager
(rc/require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)

;;; Some helpers
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(blink-cursor-blinks 0)
 '(custom-safe-themes
   '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(package-selected-packages '(zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
