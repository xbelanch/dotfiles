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

;; https://alexschroeder.ch/geocities/kensanata/colors.html
(defface bis-functions-arma-face `((t (:foreground "SeaGreen1")))  "SeaGreen1")
(defvar bis-functions-arma-face 'bis-functions-arma-face 
  "Variable for face `bis-functions-arma-face'.")

(defface magic-variables-face `((t (:foreground "orchid2")))  "orchid2")
(defvar magic-variables-face 'magic-variables-face 
  "Variable for face `magic-variables-face'.")

(defconst arma-arrays-commands
  (rx (or bol space)
      (group (or
              "append"
              "apply"
              "arrayIntersect"
              "count"
              "createHashMapFromArray"
              "deleteAt"
              "deleteRange"
              "find"
              "findAny"
              "findIf"
              "flatten"
              "forEach"
              "in"
              "insert"
              "isArray"
              "isEqualTypeAll"
              "isEqualTypeArray"
              "param"
              "params"
              "parseSimpleArray"
              "pushBack"
              "pushBackUnique"
              "resize"
              "reverse"
              "select"
              "selectMax"
              "selectMin"
              "selectRandom"
              "selectRandomWeighted"
              "set"
              "sort"
              "toArray"
              "toString"
              )
             (and (zero-or-more word)))
      space))

(defconst arma-config-commands
  (rx (or bol space)
      (group (or
              "campaignConfigFile"
              "configClasses"
              "configFile"
              "configHierarchy"
              "configName"
              "configNull"
              "configOf"
              "configProperties"
              "configSourceAddonList"
              "count"
              "getArray"
              "getMissionConfig"
              "getMissionConfigValue"
              "getNumber"
              "getText"
              "getTextRaw"
              "inheritsFrom"
              "isArray"
              "isClass"
              "isNumber"
              "isText"
              "loadConfig"
              "missionConfigFile"
              "select"
              )
             (and (zero-or-more word)))
      space))

(defconst arma-server-execution-commands
  (rx (or bol space)
      (group (or
              "addCuratorAddons"
              "addCuratorCameraArea"
              "addCuratorEditableObjects"
              "addCuratorEditingArea"
              "addCuratorPoints"
              "addPlayerScores"
              "addScore"
              "addScoreSide"
              "admin"
              "allowCuratorLogicIgnoreAreas"
              "allUsers"
              "assignCurator"
              "copyToClipboard"
              "didJIPOwner"
              "enableSimulationGlobal"
              "estimatedTimeLeft"
              "forceWeatherChange"
              "getMissionLayerEntities"
              "getUserInfo"
              "groupOwner"
              "hideObjectGlobal"
              "onPlayerConnected"
              "onPlayerDisconnected"
              "owner"
              "radioChannelCreate"
              "removeAllCuratorAddons"
              "removeAllCuratorCameraAreas"
              "removeAllCuratorEditingAreas"
              "removeCuratorAddons"
              "removeCuratorCameraArea"
              "removeCuratorEditableObjects"
              "removeCuratorEditingArea"
              "serverCommand"
              "serverNamespace"
              "setCuratorCameraAreaCeiling"
              "setCuratorCoef"
              "setCuratorEditingAreaType"
              "setCuratorWaypointCost"
              "setFog"
              "setFriend"
              "setGroupOwner"
              "setMaxLoad"
              "setOwner"
              "setRain"
              "setShotParents"
              "setTerrainHeight"
              "setTimeMultiplier"
              "setWaypointBehaviour"
              "setWaypointFormation"
              "setWaypointSpeed"
              "setWindDir"
              "skipTime"
              "turretOwner"
              "unassignCurator"
              )
             (and (zero-or-more word)))
      space))

(defconst arma-program-flow-commands
  (rx (or bol space)
      (group (or
              "assert"
              "break"
              "breakOut"
              "breakTo"
              "breakWith"
              "call"
              "canSuspend"
              "case"
              "catch"
              "continue"
              "continueWith"
              "default"
              "do"
              "else"
              "exec"
              "execFSM"
              "execVM"
              "exit"
              "exitWith"
              "fileExists"
              "for"
              "forEach"
              "forEachMember"
              "forEachMemberAgent"
              "forEachMemberTeam"
              "from"
              "goto"
              "halt"
              "if"
              "isUIContext"
              "loadFile"
              "scopeName"
              "scriptDone"
              "scriptName"
              "sleep"
              "spawn"
              "step"
              "switch"
              "terminate"
              "then"
              "throw"
              "to"
              "try"
              "uiSleep"
              "waitUntil"
              "while"
              "with"
              ))
      (or space "(" "{")))

(defconst arma-builtin-functions
  (rx (or bol space)
      (group (and
        "BIS_fnc_"
        (1+ word)
       ))
       (or space ";")))

(defconst arma-custom-functions
  (rx (or bol space)
      (group (and
        (+ (any "A-Za-z"))
        "_fnc_"
        (1+ word)
       ))
       (or space ";")))

;; https://community.bistudio.com/wiki/Magic_Variables
(defconst arma-magic-variables
  (rx (or bol space "[")
      (group (or
               "_this"
               "_x"
               "_y"
               "_exception"
               "_forEachIndex"
               "_thisArgs"
               "_thisEventHandler"
               "_thisScriptedEventHandler"
               "_thisEvent"
               "_thisFSM"
               "_thisScript"
               "_time"
               "_fnc_scriptName"
               "_fnc_scriptNameParent"
               "this"
               "thisList"
               "thisTrigger"
               ))
             (or space "," "]")))

(defconst arma-font-lock-defaults
  `(
    ;; Single quote characters
    ("\\('[[:word:]]\\)\\>" . font-lock-constant-face)
    ;; Arma builtin functions
    (,arma-builtin-functions 1 bis-functions-arma-face)
    ;; Arma custom functions
    (,arma-custom-functions 1 font-lock-function-name-face)

    ;; Arma commands
    (,arma-magic-variables 1 magic-variables-face)
    (,arma-program-flow-commands 1 font-lock-keyword-face)
    (,arma-arrays-commands 1 font-lock-builtin-face)
    (,arma-config-commands 1 font-lock-builtin-face)
    (,arma-server-execution-commands 1 font-lock-builtin-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)
    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Local variables
    ;; https://stackoverflow.com/questions/4355071/elisp-regexp-match-group-if-followed-by-other-regexp
    ("\\(^\\|\s\\|\t\\|\\[\\|\s(\\)\\(_\\w+\\)" 2 font-lock-variable-name-face)
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
