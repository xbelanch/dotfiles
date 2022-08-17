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
;; Jump into *compilation* buffer after the compilation ending
(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)

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

;; Appareance and Theme
(xba/require-package 'zenburn-theme)
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(font . "Iosevka-18"))
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(eval-after-load 'zenburn
  (set-face-attribute 'line-number nil :inherit 'default))

;; Show buffer filename on title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Visual line mode is active with text and source code files
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'prog-mode-hook #'visual-line-mode)

;; Ace-window manager
(xba/require-package 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)

;; Ido
(xba/require-package 'ido-completing-read+)
(xba/require-package 'smex)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Custom function for duplicate files on Dired
;; Stolen from https://emacs.stackexchange.com/questions/60661/how-to-duplicate-a-file-in-dired
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

;; Colorize compilation output
(require 'ansi-color)
(defun tsoding/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'tsoding/colorize-compilation-buffer)

;; c-mode
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

;;; Stolen from:
;;; https://emacs.stackexchange.com/questions/18161/how-to-replace-multiple-newlines-for-single-one-in-whole-file
(defun xba/remove-extra-blank-lines ()
    "replace multiple blank lines with a single one"
    (interactive)
    (setq orig (point))
    (goto-char (point-min))
    (while (re-search-forward "^\n+" nil t)
      (replace-match "\n")
      (forward-char 1))
     (goto-char orig))

;; Whitespace mode
;; Stolen from https://github.com/rexim/dotfiles/blob/master/.emacs
;; @TODO: Display trailing spaces when selected region
;; https://emacs.stackexchange.com/questions/54305/show-white-spaces-only-when-region-is-selected
(xba/require-package 'whitespace)
(defun xba/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 0)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'c++-mode-hook 'xba/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'xba/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'xba/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'xba/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'xba/set-up-whitespace-handling)
(add-hook 'asm-mode-hook 'xba/set-up-whitespace-handling)
(add-hook 'nasm-mode-hook 'xba/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'xba/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'xba/set-up-whitespace-handling)

;; @INFO: This makes invisible spaces
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray75")

;; Magit
(xba/require-package 'magit)
(setq magit-auto-revert-mode nil)

;; Helm
(xba/require-package 'helm)
(xba/require-package 'helm-ag)
;; Stolen from "Disable helm-find-files path autocompletion?"
;; src: https://www.reddit.com/r/emacs/comments/1q6zx2/disable_helmfindfiles_path_autocompletion/
(setq helm-ff-auto-update-initial-value nil)
(setq helm-ff-transformer-show-only-basename nil)
(setq helm-ag-base-command "ag --nocolor --ignore-case --literal --line-number --column --stats --hidden --nogroup --ignore .git")
;; @INFO: C-z disable autocompletion when you do find-file command

;; Company
(xba/require-package 'company)
(global-company-mode)

;; Yasnippet
(xba/require-package 'yasnippet)
(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))
(yas-global-mode 1)

;; Smartparens
(xba/require-package 'smartparens)
(show-smartparens-global-mode +1)
(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)

;; Multiple cursors
(xba/require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; Set custom default compile command
(setq compile-command "./build.sh")

;; Compile buffer to show in a horizontal buffer
(setq split-width-threshold nil)
(setq split-height-threshold 0)

;; Load local files
(add-to-list 'load-path "~/.emacs.local/")

;; major mode for arma scripting language
(require 'arma-mode)
(defun my-arma-mode-faces ()
  (face-remap-add-relative 'font-lock-builtin-face '(:foreground "LightGoldenrod" :weight bold))
  (face-remap-add-relative 'font-lock-keyword-face '(:foreground "light pink" :weight normal)))
(add-hook 'arma-mode-hook 'my-arma-mode-faces)
(add-hook 'arma-mode-hook 'xba/set-up-whitespace-handling)

(add-to-list 'load-path "~/.emacs.local/whitespace4r.el/")
(require 'whitespace4r)

;; Enable whitespace4r
(add-hook 'text-mode-hook #'whitespace4r-mode)
(add-hook 'prog-mode-hook #'whitespace4r-mode)
(add-hook 'markdown-mode-hook #'whitespace4r-mode)

;; Nasm
(xba/require-package 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))

;; Packages that don't require configuration
(xba/require-package 'anzu)
(xba/require-package 'mwim)
(xba/require-package 'avy)
(xba/require-package 'yaml-mode)
(xba/require-package 'lua-mode)
(xba/require-package 'graphviz-dot-mode)
(xba/require-package 'csharp-mode)
(xba/require-package 'markdown-mode)
(xba/require-package 'dockerfile-mode)
(xba/require-package 'go-mode)
(xba/require-package 'typescript-mode)
(xba/require-package 'olivetti)
(xba/require-package 'man)
(xba/require-package 'json)

;; Display line numbers
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'markdown-mode-hook #'display-line-numbers-mode)

;;; word-wrap
(defun xba/enable-word-wrap ()
  (interactive)
  (toggle-word-wrap 1))
(add-hook 'markdown-mode-hook 'xba/enable-word-wrap)

;; Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want.
(xba/require-package 'expand-region)
(global-set-key (kbd "C-;") 'er/expand-region)

;; Stolen from
;; https://emacs.stackexchange.com/questions/10348/revert-buffer-discard-unsaved-changes-without-y-n-prompt
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

;; Custom keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-q") 'kill-this-buffer)
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "C-c C-r") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)
;; C-u before of execute helm-ag to change base directory
(global-set-key (kbd "C-c h s") 'helm-ag)
(global-set-key (kbd "C-c r") 'revert-buffer-no-confirm)

;; Fix disappearing cursor
;; found at: https://emacs.stackexchange.com/questions/71413/disappearing-cursor 
(setq default-frame-alist `((cursor-color . "#FFFF00")
                            ,@default-frame-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a3e99dbdaa138996bb0c9c806bc3c3c6b4fd61d6973b946d750b555af8b7555b" "3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" default))
 '(display-line-numbers-type 'relative)
 '(package-selected-packages
   '(smex json-mode s go-mode dockerfile-mode csharp-mode expand-region paredit zenburn-theme yasnippet yaml-mode typescript-mode smartparens rainbow-delimiters olivetti nasm-mode mwim multiple-cursors move-text markdown-mode magit lua-mode js2-mode ido-completing-read+ helm-ag gruber-darker-theme graphviz-dot-mode glsl-mode company-c-headers anzu ace-window))
 '(whitespace-style
   '(face spaces tabs trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
