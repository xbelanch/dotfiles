#!/bin/bash 

cat > arma-mode.el << '_EOF'
;;; arma-mode.el --- Major Mode for editing arma files source code -*- lexical-binding: t -*-
(require 'rx)

(defvar  arma-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
	(modify-syntax-entry ?/ ". 124b" table)
	(modify-syntax-entry ?* ". 23" table)
	(modify-syntax-entry ?\n "> b" table)
    ;; Preprocessor stuff?
    (modify-syntax-entry ?# "." table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    ;; Treat <> as punctuation (needed to highlight C++ keywords
    ;; properly in template syntax)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    table))

;; Check out this https://alexschroeder.ch/geocities/kensanata/colors.html
;; if you want to costumize that colors
(defface bis-functions-arma-face `((t (:foreground "SeaGreen1")))  "SeaGreen1")
(defvar bis-functions-arma-face 'bis-functions-arma-face 
  "Variable for face `bis-functions-arma-face'.")

(defface magic-variables-face `((t (:foreground "orchid2")))  "orchid2")
(defvar magic-variables-face 'magic-variables-face 
  "Variable for face `magic-variables-face'.")

(defconst arma-builtin-functions
  (rx (or bol space)
      (group (and
        "BIS_fnc_"
        (1+ word)
       ))
       (or space ";" "}" ")" "]" )))

(defconst arma-custom-functions
  (rx (or bol space)
      (group (and
        (+ (any "A-Za-z"))
        "_fnc_"
        (1+ word)
       ))
       (or space ";" "}" ")" "]" )))

;; https://community.bistudio.com/wiki/Magic_Variables
(defconst arma-magic-variables
  (rx (or bol space "[")
      (group (or "_this" "_x" "_y" "_exception" "_forEachIndex" "_thisArgs" "_thisEventHandler" "_thisScriptedEventHandler" "_thisEvent" "_thisFSM" "_thisScript" "_time" "_fnc_scriptName" "_fnc_scriptNameParent" "this" "thisList" "thisTrigger"
               ))
             (or space "," "]")))
_EOF

# progress bar function
function prog() {
    local w=80 p=$1 size=$2;  shift
    # create a string of spaces, then change them to dots
    printf -v dots "%*s" "$(( $p*$w/$size ))" ""; dots=${dots// /.};
    # print those dots on a fixed-width space plus the percentage etc. 
    printf "\r[%-*s] %3d %% %s" "$w" "$dots" "$p" "$*"; 
}

# Scrap all the commands from Bohemia wiki site
url="https://community.bistudio.com"
path_scripting_commands="/wiki/Category:Arma_3:_Scripting_Commands"
target=$(curl -s $url$path_scripting_commands)
links=($(echo $target | grep -o -E '/wiki/Category:Command_Group:[A-Za-z_()]+'))
links_size=${#links[@]}
links_size=$(("$links_size" - 1))
const_names=()

printf "Building arma-mode.el: "
for i in "${!links[@]}"; do
    prog "$i" "$links_size" "building arma-mode.el"
    target=$(curl -s $url${links[$i]})
    printf "\n;; Parsing scripting commands from $url${links[$i]}\n" >> arma-mode.el 
    commands=($(echo $target | grep -o -E 'title="[A-Za-z]+"' | sed -n 's/^.*"\(.*\)"$/"\1"/p'))
    group_name=$(echo ${links[$i]}  |sed -n 's/.*:_\(.*\)$/\1/p' | tr "[:upper:]" "[:lower:]" | tr -d '(),' | tr "_" "-")
    const_names+=($group_name)
    printf "(defconst arma-commands-${const_names[$i]}\n" >> arma-mode.el
    echo "    (rx (or bol space \",\" \"[\" \"{\" \"(\")"$'\n'"        (group (or ${commands[@]} ))" >> arma-mode.el
    cat >> arma-mode.el << '_EOF'
    (or space "," "]" "}" ")" ";" )))
_EOF
done

# Include font-lock-defaults
cat >> arma-mode.el << '_EOF'

(defconst arma-font-lock-defaults
  `(
    ;; Single quote characters
    ("\\('[[:word:]]\\)\\>" . font-lock-constant-face)
    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)
    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)
    ;; Magic variables
    (,arma-magic-variables 1 magic-variables-face)
    ;; Local variables
    ;; https://stackoverflow.com/questions/4355071/elisp-regexp-match-group-if-followed-by-other-regexp
    ("\\(^\\|\s\\|\t\\|\\[\\|\s(\\)\\(_\\w+\\)" 2 font-lock-variable-name-face)
    ;; Arma builtin functions
    (,arma-builtin-functions 1 bis-functions-arma-face)
    ;; Arma custom functions
    (,arma-custom-functions 1 font-lock-function-name-face)
    ;; Arma scripting commands
_EOF

for name in "${const_names[@]}"; do
    echo "    (,arma-commands-$name 1 font-lock-builtin-face)" >> arma-mode.el
done

# Append last part of file
cat >> arma-mode.el << '_EOF'
))

;; stolen from @tsoding simpc.el
;; works like a charm!
;; https://github.com/rexim/dotfiles/blob/master/.emacs.local/simpc-mode.el
(defun simpc--space-prefix-len (line)
  (- (length line)
     (length (string-trim-left line))))

(defun simpc--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun simpc--desired-indentation ()
  (let ((cur-line (string-trim-right (thing-at-point 'line t)))
        (prev-line (string-trim-right (simpc--previous-non-empty-line)))
        (indent-len 4))
    (cond
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      (simpc--space-prefix-len prev-line))
     ((string-suffix-p "{" prev-line)
      (+ (simpc--space-prefix-len prev-line) indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- (simpc--space-prefix-len prev-line) indent-len) 0))
     (t (simpc--space-prefix-len prev-line)))))

(defun simpc-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((current-indentation
            (simpc--space-prefix-len (thing-at-point 'line t)))
           (desired-indentation
            (simpc--desired-indentation))
           (n (max (- (current-column) current-indentation) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

(define-derived-mode arma-mode prog-mode "arma"
  "Major Mode for editing arma source code."
  :syntax-table arma-mode-syntax-table
  (setq font-lock-defaults '(arma-font-lock-defaults))
  (setq-local indent-line-function 'simpc-indent-line)
  (setq-local comment-start "// "))

(add-to-list 'auto-mode-alist '("\\.sqf\\'" . arma-mode))

(provide 'arma-mode)

_EOF
