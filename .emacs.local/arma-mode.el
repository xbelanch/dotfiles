;;; arma-mode.el --- Major Mode for editing arma files source code -*- lexical-binding: t -*-
(require 'rx)

(defconst arma-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; C/C++ style comments
	(modify-syntax-entry ?/ ". 124b")
	(modify-syntax-entry ?* ". 23")
	(modify-syntax-entry ?\n "> b")
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"")
    (syntax-table))
  "Syntax table for `arma-mode'.")

(defvar arma-keywords
  '("and"  "do" "else" "exit" "exitWith" "false" "for" "forEach" "forEachMember" "forEachMemberAgent" "forEachMemberTeam" "from" "if" "in" "nil" "not" "or" "then" "to" "true" "try" "waitUntil" "with" "while"))

;; https://alexschroeder.ch/geocities/kensanata/colors.html
(defface bis-functions-arma-face `((t (:foreground "SeaGreen1")))  "SeaGreen1")
(defvar bis-functions-arma-face 'bis-functions-arma-face 
  "Variable for face `bis-functions-arma-face'.")

(defun arma-commands-rx (commands)
  "build commands regexp"
  (regexp-opt commands t))

(defconst arma-font-lock-defaults
  `(
    ;; Keywords
    (,(regexp-opt arma-keywords 'symbols) . font-lock-keyword-face)
    ;; Single quote characters
    ("\\('[[:word:]]\\)\\>" . font-lock-constant-face)
    ;; Arma BIS functions
    ("\s\\(BIS_fnc_\\w+\\)" . bis-functions-arma-face)
    ;; Custom functions
    ("\s\\(\\w+_fnc_\\w+\\)" . font-lock-function-name-face)
    ;; Arma commands
    (,(rx (or
           bol
           space)
          (group (or
                  "abs"
                  "accTime"
                  "acos"
                  "action"
                  "activate"
                  "add"
                  "admin"
                  "agent"
                  "agents"
                  "air"
                  "all"
                  "anim"
                  "assign"
                  "attach"
                  "attack"
                  "backpack"
                  "bounding"
                  "break"
                  "building"
                  "call"
                  "cam"
                  "can"
                  "class"
                  "clear"
                  "close"
                  "command"
                  "compile"
                  "config"
                  "copy"
                  "create"
                  "ct"
                  "curator"
                  "current"
                  "delete"
                  "diag "
                  "dialog"
                  "disable"
                  "display"
                  "distance"
                  "difficulty"
                  "do"
                  "draw"
                  "dynamic"
                  "east"
                  "echo"
                  "enable"
                  "end"
                  "env"
                  "exec"
                  "face"
                  "faction"
                  "fade"
                  "find"
                  "fire"
                  "flag"
                  "floor"
                  "fog"
                  "force"
                  "format"
                  "fuel"
                  "full"
                  "gear"
                  "get"
                  "global"
                  "group"
                  "halt"
                  "has"
                  "hc"
                  "hint"
                  "in"
                  "is"
                  "join"
                  "kb"
                  "land"
                  "lb"
                  "leader"
                  "line"
                  "lnb"
                  "load"
                  "look"
                  "magazine"
                  "marker"
                  "max"
                  "menu"
                  "min"
                  "mine"
                  "mod"
                  "model"
                  "move"
                  "name"
                  "near"
                  "obj"
                  "on"
                  "open"
                  "par"
                  "play"
                  "pp"
                  "preload"
                  "push"
                  "query"
                  "rad"
                  "regex"
                  "remote"
                  "remove"
                  "road"
                  "role"
                  "rope"
                  "safe"
                  "say"
                  "screen"
                  "select"
                  "set"
                  "show"
                  "skip"
                  "sleep"
                  "switch"
                  "synch"
                  "target"
                  "task"
                  "team"
                  "text"
                  "to"
                  "trigger"
                  "tv"
                  "ui"
                  "uniform"
                  "unit"
                  "vector"
                  "vest"
                  "view"
                  "visible"
                  "wait"
                  "waypoint"
                  "weapon"
                  "wind"
                  "with"
                  "world"
                  )
                 (and
                  (zero-or-more word))
                 )
          space) 1 font-lock-builtin-face)
    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)
    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)
    ;; Local variables
    ;; https://stackoverflow.com/questions/4355071/elisp-regexp-match-group-if-followed-by-other-regexp
    ("\\(^\\|\s\\|\t\\|\\[\\|\s(\\)\\(_\\w+\\)" 2 font-lock-variable-name-face)
    ))

;;;###autoload
(define-derived-mode arma-mode prog-mode "arma"
  "Major Mode for editing arma source code."
  :syntax-table arma-mode-syntax-table
  (setq font-lock-defaults '(arma-font-lock-defaults))
  (setq-local comment-start "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sqf\\'" . arma-mode))

(provide 'arma-mode)
