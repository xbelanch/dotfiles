;;; arma-mode.el --- Major Mode for editing arma files source code -*- lexical-binding: t -*-
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

(eval-and-compile
  (defconst arma-keywords
  '("and"  "do" "else" "exit" "exitWith" "false" "for" "forEach" "forEachMember" "forEachMemberAgent" "forEachMemberTeam" "from" "if" "in" "nil" "not" "or" "then" "to" "true" "try" "waitUntil" "with" "while")))

(eval-and-compile
  (defconst arma-commands
    '("call" "getArray" "createMarker" "setMarkerColor" "setMarkerType" "setMarkerText" "format" "nearestLocations" "nearObjects" "getPos" "setDate" "createVehicle" "position" "landAt" "private" "setDamage" "sleep")))

(eval-and-compile
  (defconst arma-functions
    '("BIS_fnc_param" "BIS_fnc_spawnVehicle")))

(defconst arma-font-lock-defaults
  `(
    ;; Keywords
    (,(regexp-opt arma-keywords 'symbols) . font-lock-keyword-face)
    ;; Single quote characters
    ("\\('[[:word:]]\\)\\>" . font-lock-constant-face)
    ;; Functions
    (,(regexp-opt arma-functions) . font-lock-function-name-face)
    ;; Custom functions
    ("\s\\(\\w+_fnc_\\w+\\)" . font-lock-function-name-face)
    ;; Commands
    (,(regexp-opt arma-commands) . font-lock-builtin-face)
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
