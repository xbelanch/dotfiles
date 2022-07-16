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

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Animations
(defconst arma-commands-animations
    (rx (or bol space "," "[" "{" "(")
        (group (or "animate" "animateBay" "animateDoor" "animatePylon" "animateSource" "animationNames" "animationPhase" "animationSourcePhase" "animationState" "doorPhase" "elevatePeriscope" "gestureState" "getAnimAimPrecision" "getAnimSpeedCoef" "moveTime" "periscopeElevation" "playAction" "playActionNow" "playGesture" "playMove" "playMoveNow" "setAnimSpeedCoef" "setFaceAnimation" "switchAction" "switchGesture" "switchMove" "useAudioTimeForMoves" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Arrays
(defconst arma-commands-arrays
    (rx (or bol space "," "[" "{" "(")
        (group (or "append" "apply" "arrayIntersect" "count" "createHashMapFromArray" "deleteAt" "deleteRange" "find" "findAny" "findIf" "flatten" "forEach" "in" "insert" "isArray" "isEqualTypeAll" "isEqualTypeArray" "param" "params" "parseSimpleArray" "pushBack" "pushBackUnique" "resize" "reverse" "select" "selectMax" "selectMin" "selectRandom" "selectRandomWeighted" "set" "sort" "toArray" "toString" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Artillery
(defconst arma-commands-artillery
    (rx (or bol space "," "[" "{" "(")
        (group (or "commandArtilleryFire" "doArtilleryFire" "enableEngineArtillery" "getArtilleryAmmo" "getArtilleryComputerSettings" "getArtilleryETA" "inRangeOfArtillery" "shownArtilleryComputer" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Briefing
(defconst arma-commands-briefing
    (rx (or bol space "," "[" "{" "(")
        (group (or "allDiaryRecords" "allDiarySubjects" "briefingName" "cancelSimpleTaskDestination" "createDiaryLink" "createDiaryRecord" "createDiarySubject" "createSimpleTask" "createTask" "currentTask" "currentTasks" "debriefingText" "diaryRecordNull" "diarySubjectExists" "disableDebriefingStats" "enableDebriefingStats" "getDebriefingText" "objStatus" "onBriefingGear" "onBriefingGroup" "onBriefingNotes" "onBriefingPlan" "onBriefingTeamSwitch" "priority" "processDiaryLink" "registeredTasks" "registerTask" "removeDiaryRecord" "removeDiarySubject" "removeSimpleTask" "selectDiarySubject" "sendTask" "sendTaskResult" "setCurrentTask" "setDebriefingText" "setDiaryRecordText" "setDiarySubjectPicture" "setSimpleTaskAlwaysVisible" "setSimpleTaskCustomData" "setSimpleTaskDescription" "setSimpleTaskDestination" "setSimpleTaskTarget" "setSimpleTaskType" "setTaskMarkerOffset" "setTaskResult" "setTaskState" "simpleTasks" "taskAlwaysVisible" "taskChildren" "taskCompleted" "taskCustomData" "taskDescription" "taskDestination" "taskHint" "taskMarkerOffset" "taskName" "taskNull" "taskParent" "taskResult" "taskState" "taskType" "type" "unregisterTask" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Broken_Commands
(defconst arma-commands-broken-commands
    (rx (or bol space "," "[" "{" "(")
        (group (or "addPublicVariableEventHandler" "addWeaponGlobal" "allSites" "camPrepareBank" "camPrepareDir" "camPrepareDive" "camPrepareFovRange" "camSetBank" "camSetDive" "camSetFovRange" "createSite" "createSoundSource" "createTarget" "debugFSM" "debugLog" "deleteSite" "deleteTarget" "disableDebriefingStats" "echo" "enemy" "findCover" "getPersonUsedDLCs" "halt" "isHideBehindScripted" "magazineTurretAmmo" "moveTarget" "netObjNull" "removeClothing" "setFaceAnimation" "setHideBehind" "setMagazineTurretAmmo" "setPlayable" "setSystemOfUnits" "setVehicleId" "showWarrant" "simulSetHumidity" "textLog" "textLogFormat" "unlockAchievement" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Camera_Control
(defconst arma-commands-camera-control
    (rx (or bol space "," "[" "{" "(")
        (group (or "Curator" "addCamShake" "apertureParams" "camCommand" "camCommit" "camCommitPrepared" "camCommitted" "camConstuctionSetParams" "camCreate" "camDestroy" "cameraEffect" "cameraEffectEnableHUD" "cameraInterest" "cameraOn" "cameraView" "camPreload" "camPreloaded" "camPrepareBank" "camPrepareDir" "camPrepareDive" "camPrepareFocus" "camPrepareFov" "camPrepareFovRange" "camPreparePos" "camPrepareRelPos" "camPrepareTarget" "camSetBank" "camSetDir" "camSetDive" "camSetFocus" "camSetFov" "camSetFovRange" "camSetPos" "camSetRelPos" "camSetTarget" "camTarget" "camUseNVG" "directionStabilizationEnabled" "enableCamShake" "enableDirectionStabilization" "enableEndDialog" "lockCameraTo" "lockedCameraTo" "positionCameraToWorld" "ppEffectAdjust" "ppEffectCommit" "ppEffectCommitted" "ppEffectCreate" "ppEffectDestroy" "ppEffectEnable" "ppEffectEnabled" "ppEffectForceInNVG" "preloadCamera" "resetCamShake" "setAperture" "setApertureNew" "setCameraEffect" "setCameraInterest" "setCamShakeDefParams" "setCamShakeParams" "setCamUseTI" "setDefaultCamera" "setHorizonParallaxCoef" "setPiPEffect" "showCinemaBorder" "switchCamera" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Config
(defconst arma-commands-config
    (rx (or bol space "," "[" "{" "(")
        (group (or "campaignConfigFile" "configClasses" "configFile" "configHierarchy" "configName" "configNull" "configOf" "configProperties" "configSourceAddonList" "count" "getArray" "getMissionConfig" "getMissionConfigValue" "getNumber" "getText" "getTextRaw" "inheritsFrom" "isArray" "isClass" "isNumber" "isText" "loadConfig" "missionConfigFile" "select" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Containers
(defconst arma-commands-containers
    (rx (or bol space "," "[" "{" "(")
        (group (or "addBackpack" "addBackpackCargo" "addBackpackCargoGlobal" "addItemToBackpack" "addMagazineCargo" "addMagazineCargoGlobal" "addWeaponCargo" "addWeaponCargoGlobal" "backpack" "backpackCargo" "backpackContainer" "backpackItems" "backpackMagazines" "backpackSpaceFor" "canAddItemToBackpack" "clearAllItemsFromBackpack" "clearBackpackCargo" "clearBackpackCargoGlobal" "clearItemCargo" "clearItemCargoGlobal" "clearMagazineCargo" "clearMagazineCargoGlobal" "clearWeaponCargo" "clearWeaponCargoGlobal" "everyBackpack" "firstBackpack" "forceAddUniform" "getBackpackCargo" "getMagazineCargo" "getWeaponCargo" "loadBackpack" "magazineCargo" "removeAllItemsWithMagazines" "removeBackpack" "removeItemFromBackpack" "removeUniform" "removeVest" "uniform" "uniformContainer" "uniformMagazines" "unitBackpack" "vest" "vestContainer" "weaponAccessoriesCargo" "weaponCargo" "weaponsItemsCargo" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Conversations
(defconst arma-commands-conversations
    (rx (or bol space "," "[" "{" "(")
        (group (or "Conversations" "kbAddDatabase" "kbAddDatabaseTargets" "kbAddTopic" "kbHasTopic" "kbReact" "kbRemoveTopic" "kbTell" "kbWasSaid" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Curator
(defconst arma-commands-curator
    (rx (or bol space "," "[" "{" "(")
        (group (or "addCuratorAddons" "addCuratorCameraArea" "addCuratorEditableObjects" "addCuratorEditingArea" "addCuratorPoints" "allCurators" "allowCuratorLogicIgnoreAreas" "assignCurator" "curatorAddons" "curatorCamera" "curatorCameraArea" "curatorCameraAreaCeiling" "curatorCoef" "curatorEditableObjects" "curatorEditingArea" "curatorEditingAreaType" "curatorMouseOver" "curatorPoints" "curatorRegisteredObjects" "curatorSelected" "curatorWaypointCost" "getAssignedCuratorLogic" "getAssignedCuratorUnit" "objectCurators" "openCuratorInterface" "removeAllCuratorAddons" "removeAllCuratorCameraAreas" "removeAllCuratorEditingAreas" "removeCuratorAddons" "removeCuratorCameraArea" "removeCuratorEditableObjects" "removeCuratorEditingArea" "setCuratorCameraAreaCeiling" "setCuratorCoef" "setCuratorEditingAreaType" "setCuratorWaypointCost" "showCuratorCompass" "shownCuratorCompass" "unassignCurator" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Custom_Panels
(defconst arma-commands-custom-panels
    (rx (or bol space "," "[" "{" "(")
        (group (or "enableInfoPanelComponent" "infoPanel" "infoPanelComponentEnabled" "infoPanelComponents" "infoPanels" "setInfoPanel" "shownUAVFeed" "showUAVFeed" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Diagnostic
(defconst arma-commands-diagnostic
    (rx (or bol space "," "[" "{" "(")
        (group (or "echo" "enableDiagLegend" "exportJIPMessages" "getTerrainHeight" "getTerrainInfo" "isFilePatchingEnabled" "logEntities" "logNetwork" "logNetworkTerminate" "setCustomMissionData" "setTerrainHeight" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Difficulty
(defconst arma-commands-difficulty
    (rx (or bol space "," "[" "{" "(")
        (group (or "cadetMode" "difficulty" "difficultyEnabled" "difficultyEnabledRTD" "difficultyOption" "disableMapIndicators" "forceCadetDifficulty" "hintCadet" "isInstructorFigureEnabled" "isTutHintsEnabled" "missionDifficulty" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Dynamic_Simulation
(defconst arma-commands-dynamic-simulation
    (rx (or bol space "," "[" "{" "(")
        (group (or "canTriggerDynamicSimulation" "dynamicSimulationDistance" "dynamicSimulationDistanceCoef" "dynamicSimulationEnabled" "dynamicSimulationSystemEnabled" "enableDynamicSimulation" "enableDynamicSimulationSystem" "setDynamicSimulationDistance" "setDynamicSimulationDistanceCoef" "triggerDynamicSimulation" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Eden_Editor
(defconst arma-commands-eden-editor
    (rx (or bol space "," "[" "{" "(")
        (group (or "getMissionLayerEntities" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Environment
(defconst arma-commands-environment
    (rx (or bol space "," "[" "{" "(")
        (group (or "ambientTemperature" "date" "dateToNumber" "dayTime" "enableEnvironment" "enableTraffic" "environmentEnabled" "fog" "fogForecast" "fogParams" "forceWeatherChange" "getObjectViewDistance" "getShadowDistance" "getTerrainGrid" "gusts" "humidity" "initAmbientLife" "lightnings" "moonIntensity" "moonPhase" "nextWeatherChange" "numberToDate" "overcast" "overcastForecast" "rain" "rainbow" "rainParams" "setDate" "setFog" "setGusts" "setHumidity" "setLightnings" "setLocalWindParams" "setObjectViewDistance" "setOvercast" "setRain" "setRainbow" "setShadowDistance" "setSimulWeatherLayers" "setTerrainGrid" "setTimeMultiplier" "setTrafficDensity" "setTrafficDistance" "setTrafficGap" "setTrafficSpeed" "setViewDistance" "setWaves" "setWind" "setWindDir" "setWindForce" "setWindStr" "simulCloudDensity" "simulCloudOcclusion" "simulInClouds" "simulSetHumidity" "simulWeatherSync" "skipTime" "sunOrMoon" "timeMultiplier" "viewDistance" "waves" "wind" "windDir" "windStr" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Event_Handlers
(defconst arma-commands-event-handlers
    (rx (or bol space "," "[" "{" "(")
        (group (or "addEventHandler" "addMissionEventHandler" "addMPEventHandler" "addMusicEventHandler" "addPublicVariableEventHandler" "addUserActionEventHandler" "ctrlAddEventHandler" "ctrlRemoveAllEventHandlers" "ctrlRemoveEventHandler" "ctrlSetEventHandler" "displayAddEventHandler" "displayRemoveAllEventHandlers" "displayRemoveEventHandler" "displaySetEventHandler" "editorSetEventHandler" "getEventHandlerInfo" "inGameUISetEventHandler" "onBriefingGear" "onBriefingGroup" "onBriefingNotes" "onBriefingPlan" "onBriefingTeamSwitch" "onCommandModeChanged" "onEachFrame" "onGroupIconClick" "onGroupIconOverEnter" "onGroupIconOverLeave" "onHCGroupSelectionChanged" "onMapSingleClick" "onPlayerConnected" "onPlayerDisconnected" "onPreloadFinished" "onPreloadStarted" "onTeamSwitch" "removeAllEventHandlers" "removeAllMissionEventHandlers" "removeAllMusicEventHandlers" "removeAllUserActionEventHandlers" "removeEventHandler" "removeMissionEventHandler" "removeMPEventHandler" "removeMusicEventHandler" "removeUserActionEventHandler" "setMusicEventHandler" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Flags
(defconst arma-commands-flags
    (rx (or bol space "," "[" "{" "(")
        (group (or "flag" "flagAnimationPhase" "flagOwner" "flagSide" "flagTexture" "forceFlagTexture" "getForcedFlagTexture" "setFlagAnimationPhase" "setFlagOwner" "setFlagSide" "setFlagTexture" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Game_
(defconst arma-commands-game-
    (rx (or bol space "," "[" "{" "(")
        (group (or  ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Groups
(defconst arma-commands-groups
    (rx (or bol space "," "[" "{" "(")
        (group (or "allGroups" "allowFleeing" "assignTeam" "attackEnabled" "combatBehaviour" "combatMode" "commandMove" "commandStop" "commandTarget" "createGroup" "deleteGroup" "deleteGroupWhenEmpty" "dissolveTeam" "enableAttack" "forgetTarget" "formation" "formationDirection" "formationMembers" "formationTask" "group" "groupFromNetId" "groupId" "grpNull" "isFormationLeader" "isGroupDeletedWhenEmpty" "join" "joinAs" "joinAsSilent" "joinSilent" "leader" "leaveVehicle" "onBriefingGroup" "resetSubgroupDirection" "selectLeader" "setBehaviour" "setBehaviourStrong" "setCombatBehaviour" "setCombatMode" "setFormation" "setFormDir" "setGroupId" "setGroupIdGlobal" "setSpeedMode" "speedMode" "unassignTeam" "units" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_GUI_Control
(defconst arma-commands-gui-control
    (rx (or bol space "," "[" "{" "(")
        (group (or "activeTitleEffectParams" "allActiveTitleEffects" "allControls" "allCutLayers" "allDisplays" "buttonAction" "buttonSetAction" "cbChecked" "cbSetChecked" "closeDialog" "closeDisplay" "controlNull" "controlsGroupCtrl" "createDialog" "createDisplay" "createGearDialog" "createMissionDisplay" "createMPCampaignDisplay" "ctrlActivate" "ctrlAutoScrollDelay" "ctrlAutoScrollRewind" "ctrlAutoScrollSpeed" "ctrlBackgroundColor" "ctrlChecked" "ctrlClassName" "ctrlCommit" "ctrlCommitted" "ctrlCreate" "ctrlDelete" "ctrlEnable" "ctrlEnabled" "ctrlFade" "ctrlFontHeight" "ctrlForegroundColor" "ctrlIDC" "ctrlIDD" "ctrlParent" "ctrlParentControlsGroup" "ctrlScrollValues" "ctrlSetActiveColor" "ctrlSetAngle" "ctrlSetAutoScrollDelay" "ctrlSetAutoScrollRewind" "ctrlSetAutoScrollSpeed" "ctrlSetBackgroundColor" "ctrlSetChecked" "ctrlSetDisabledColor" "ctrlSetFade" "ctrlSetFocus" "ctrlSetFont" "ctrlSetFontHeightSecondary" "ctrlSetFontSecondary" "ctrlSetForegroundColor" "ctrlSetScrollValues" "ctrlSetShadow" "ctrlSetStructuredText" "ctrlSetText" "ctrlSetTextColor" "ctrlSetTextColorSecondary" "ctrlSetTextSecondary" "ctrlSetTextSelection" "ctrlSetTooltip" "ctrlSetTooltipColorBox" "ctrlSetTooltipColorShade" "ctrlSetTooltipColorText" "ctrlSetTooltipMaxWidth" "ctrlSetURL" "ctrlSetURLOverlayMode" "ctrlShadow" "ctrlShow" "ctrlShown" "ctrlStyle" "ctrlText" "ctrlTextColor" "ctrlTextHeight" "ctrlTextSecondary" "ctrlTextSelection" "ctrlTextWidth" "ctrlTooltip" "ctrlType" "ctrlURL" "ctrlURLOverlayMode" "ctrlVisible" "cutFadeOut" "cutObj" "cutRsc" "cutText" "dialog" "disableSerialization" "displayChild" "displayCtrl" "displayNull" "displayParent" "findDisplay" "focusedCtrl" "gearIDCAmmoCount" "gearSlotAmmoCount" "gearSlotData" "getTextWidth" "getUserMFDText" "htmlLoad" "image" "isSteamOverlayEnabled" "isUIContext" "lbTooltip" "preloadTitleObj" "preloadTitleRsc" "progressPosition" "progressSetPosition" "setUserMFDText" "sliderPosition" "sliderRange" "sliderSetPosition" "sliderSetRange" "sliderSetSpeed" "sliderSpeed" "titleCut" "titleFadeOut" "titleObj" "titleRsc" "titleText" "uiNamespace" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_HashMap
(defconst arma-commands-hashmap
    (rx (or bol space "," "[" "{" "(")
        (group (or "HashMap" "apply" "count" "createHashMap" "createHashMapFromArray" "deleteAt" "forEach" "get" "getOrDefault" "hashValue" "in" "insert" "keys" "merge" "set" "toArray" "values" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_High_Command
(defconst arma-commands-high-command
    (rx (or bol space "," "[" "{" "(")
        (group (or "addGroupIcon" "clearGroupIcons" "getGroupIcon" "getGroupIconParams" "getGroupIcons" "groupIconSelectable" "groupIconsVisible" "hcAllGroups" "hcGroupParams" "hcLeader" "hcRemoveAllGroups" "hcRemoveGroup" "hcSelected" "hcSelectGroup" "hcSetGroup" "hcShowBar" "hcShownBar" "onCommandModeChanged" "onGroupIconClick" "onGroupIconOverEnter" "onGroupIconOverLeave" "onHCGroupSelectionChanged" "removeGroupIcon" "setGroupIcon" "setGroupIconParams" "setGroupIconsSelectable" "setGroupIconsVisible" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Interaction
(defconst arma-commands-interaction
    (rx (or bol space "," "[" "{" "(")
        (group (or "action" "actionIDs" "actionKeys" "actionKeysEx" "actionKeysImages" "actionKeysNames" "actionKeysNamesArray" "actionName" "actionParams" "addAction" "commandingMenu" "disableUserInput" "drawLaser" "forcedMap" "forceMap" "getUserMFDValue" "groupSelectedUnits" "groupSelectUnit" "hint" "hintC" "hintCadet" "hintSilent" "HUDMovementLevels" "inGameUISetEventHandler" "inputAction" "inputController" "inputMouse" "isActionMenuVisible" "openGPS" "openMap" "removeAction" "removeAllActions" "setCompassOscillation" "setHUDMovementLevels" "setRadioMsg" "setUserActionText" "setUserMFDValue" "showCommandingMenu" "showCompass" "showGPS" "showHUD" "shownCompass" "shownGPS" "shownHUD" "shownPad" "shownRadio" "shownScoretable" "shownWarrant" "shownWatch" "showPad" "showRadio" "showScoretable" "showWarrant" "showWatch" "userInputDisabled" "visibleCompass" "visibleGPS" "visibleMap" "visibleScoretable" "visibleWatch" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Leaderboards
(defconst arma-commands-leaderboards
    (rx (or bol space "," "[" "{" "(")
        (group (or "leaderboardDeInit" "leaderboardGetRows" "leaderboardInit" "leaderboardRequestRowsFriends" "leaderboardRequestRowsGlobal" "leaderboardRequestRowsGlobalAroundUser" "leaderboardsRequestUploadScore" "leaderboardsRequestUploadScoreKeepBest" "leaderboardState" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Lights
(defconst arma-commands-lights
    (rx (or bol space "," "[" "{" "(")
        (group (or "apertureParams" "drawLaser" "enableGunLights" "getLighting" "getLightingAt" "isCollisionLightOn" "isIRLaserOn" "isLightOn" "lightAttachObject" "lightDetachObject" "lightIsOn" "setAperture" "setApertureNew" "setCollisionLight" "setLightAmbient" "setLightAttenuation" "setLightBrightness" "setLightColor" "setLightConePars" "setLightDayLight" "setLightFlareMaxDistance" "setLightFlareSize" "setLightIntensity" "setLightIR" "setLightUseFlare" "setLightVolumeShape" "setPilotLight" "switchLight" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Localization
(defconst arma-commands-localization
    (rx (or bol space "," "[" "{" "(")
        (group (or "getTextRaw" "isLocalized" "localize" "WFSideText" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Locations
(defconst arma-commands-locations
    (rx (or bol space "," "[" "{" "(")
        (group (or "Location" "attachObject" "className" "createLocation" "deleteLocation" "direction" "drawLocation" "importance" "in" "inArea" "inAreaArray" "locationNull" "locationPosition" "name" "nearestLocation" "nearestLocations" "nearestLocationWithDubbing" "rectangular" "setDirection" "setImportance" "setName" "setPosition" "setRectangular" "setSide" "setSize" "setSpeech" "setText" "setType" "size" "text" "type" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Map
(defconst arma-commands-map
    (rx (or bol space "," "[" "{" "(")
        (group (or "forcedMap" "forceMap" "getElevationOffset" "getObjectID" "mapAnimAdd" "mapAnimClear" "mapAnimCommit" "mapAnimDone" "mapGridPosition" "onMapSingleClick" "openMap" "showMap" "shownMap" "visibleMap" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Markers
(defconst arma-commands-markers
    (rx (or bol space "," "[" "{" "(")
        (group (or "allMapMarkers" "createMarker" "createMarkerLocal" "deleteMarker" "deleteMarkerLocal" "getMarkerColor" "getMarkerPos" "getMarkerSize" "getMarkerType" "getPlayerID" "inArea" "inAreaArray" "markerAlpha" "markerBrush" "markerChannel" "markerColor" "markerDir" "markerPolyline" "markerPos" "markerShadow" "markerShape" "markerSize" "markerText" "markerType" "setMarkerAlpha" "setMarkerAlphaLocal" "setMarkerBrush" "setMarkerBrushLocal" "setMarkerColor" "setMarkerColorLocal" "setMarkerDir" "setMarkerDirLocal" "setMarkerPolyline" "setMarkerPolylineLocal" "setMarkerPos" "setMarkerPosLocal" "setMarkerShadow" "setMarkerShadowLocal" "setMarkerShape" "setMarkerShapeLocal" "setMarkerSize" "setMarkerSizeLocal" "setMarkerText" "setMarkerTextLocal" "setMarkerType" "setMarkerTypeLocal" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Math
(defconst arma-commands-math
    (rx (or bol space "," "[" "{" "(")
        (group (or "abs" "and" "bezierInterpolation" "ceil" "decayGraphValues" "exp" "false" "finite" "floor" "getGraphValues" "linearConversion" "ln" "log" "matrixMultiply" "matrixTranspose" "max" "min" "mod" "not" "or" "random" "round" "sqrt" "toFixed" "true" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Mines
(defconst arma-commands-mines
    (rx (or bol space "," "[" "{" "(")
        (group (or "addOwnedMine" "allMines" "createMine" "detectedMines" "getAllOwnedMines" "mineActive" "mineDetectedBy" "removeAllOwnedMines" "removeOwnedMine" "revealMine" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Mission_Information
(defconst arma-commands-mission-information
    (rx (or bol space "," "[" "{" "(")
        (group (or "activateKey" "dayTime" "deActivateKey" "enableEndDialog" "enableSaving" "endMission" "estimatedEndServerTime" "estimatedTimeLeft" "failMission" "forceEnd" "getMissionConfig" "getMissionConfigValue" "getMissionDLCs" "getMissionLayerEntities" "getMissionLayers" "getMissionPath" "getWorld" "isKeyActive" "isSaving" "isSteamMission" "loadGame" "loadStatus" "markAsFinishedOnSteam" "missionDifficulty" "missionEnd" "missionName" "missionNameSource" "missionStart" "missionVersion" "saveGame" "saveStatus" "savingEnabled" "selectBestPlaces" "serverTime" "setDate" "setTimeMultiplier" "time" "timeMultiplier" "worldName" "worldSize" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Mods_and_Addons
(defconst arma-commands-mods-and-addons
    (rx (or bol space "," "[" "{" "(")
        (group (or "activateAddons" "activatedAddons" "addonFiles" "allAddonsInfo" "configSourceAddonList" "configSourceMod" "configSourceModList" "getLoadedModsInfo" "modParams" "unitAddons" "verifySignature" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Multiplayer
(defconst arma-commands-multiplayer
    (rx (or bol space "," "[" "{" "(")
        (group (or "addMPEventHandler" "addPlayerScores" "addPublicVariableEventHandler" "addScore" "addScoreSide" "admin" "allPlayers" "allUsers" "clientOwner" "didJIP" "didJIPOwner" "disableRemoteSensors" "estimatedEndServerTime" "estimatedTimeLeft" "exportJIPMessages" "forceRespawn" "getClientState" "getClientStateNumber" "getPlayerID" "getPlayerScores" "getPlayerUID" "getPlayerUIDOld" "getUserInfo" "getVariable" "groupFromNetId" "groupOwner" "hasInterface" "hostMission" "isDedicated" "isMultiplayer" "isMultiplayerSolo" "isPlayer" "isRemoteExecuted" "isRemoteExecutedJIP" "isServer" "local" "localNamespace" "logNetwork" "logNetworkTerminate" "netId" "objectFromNetId" "onPlayerConnected" "onPlayerDisconnected" "owner" "playableSlotsNumber" "playableUnits" "playerRespawnTime" "playersNumber" "publicVariable" "publicVariableClient" "publicVariableServer" "remoteExec" "remoteExecCall" "remoteExecutedOwner" "removeAllMPEventHandlers" "removeMPEventHandler" "removeSwitchableUnit" "respawnVehicle" "roleDescription" "score" "scoreSide" "selectPlayer" "sendAUMessage" "sendUDPMessage" "serverCommand" "serverCommandAvailable" "serverCommandExecutable" "serverName" "serverNamespace" "serverTime" "setGroupOwner" "setOwner" "setPlayable" "setPlayerRespawnTime" "setVariable" "switchableUnits" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Object_Detection
(defconst arma-commands-object-detection
    (rx (or bol space "," "[" "{" "(")
        (group (or "agents" "allCurators" "allDead" "allDeadMen" "allGroups" "allMissionObjects" "allPlayers" "allSimpleObjects" "allUnits" "allUnitsUAV" "allUsers" "countEnemy" "countSide" "countType" "cursorObject" "cursorTarget" "entities" "findNearestEnemy" "getCursorObjectParams" "getUserInfo" "nearEntities" "nearestBuilding" "nearestMines" "nearestObject" "nearestObjects" "nearestTerrainObjects" "nearObjects" "nearObjectsReady" "nearRoads" "nearSupplies" "nearTargets" "object" "playableUnits" "switchableUnits" "targets" "targetsQuery" "units" "unitsBelowHeight" "vehicles" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Object_Manipulation
(defconst arma-commands-object-manipulation
    (rx (or bol space "," "[" "{" "(")
        (group (or "addForce" "addPlayerScores" "addRating" "addToRemainsCollector" "addTorque" "addVehicle" "aimedAtTarget" "airplaneThrottle" "AISFinishHeal" "alive" "allLODs" "allObjects" "allowCrewInImmobile" "allowDamage" "allowDammage" "allowFleeing" "allowGetIn" "assignedCargo" "assignedCommander" "assignedDriver" "assignedGunner" "assignedTarget" "assignedTeam" "assignedVehicle" "assignedVehicleRole" "attachedObject" "attachedObjects" "attachedTo" "attachObject" "attachTo" "awake" "boundingBox" "boundingBoxReal" "boundingCenter" "brakesDisabled" "buildingExit" "calculatePath" "canMove" "canStand" "canUnloadInCombat" "captive" "captiveNum" "clearVehicleInit" "collisionDisabledWith" "commander" "createAgent" "createSimpleObject" "createUnit" "createVehicle" "createVehicleCrew" "createVehicleLocal" "crew" "currentPilot" "damage" "deleteCollection" "deleteStatus" "deleteVehicle" "deleteVehicleCrew" "detach" "direction" "disableBrakes" "disableCollisionWith" "driver" "elevatePeriscope" "emptyPositions" "enableCollisionWith" "enableCopilot" "enableMimics" "enableSimulation" "enableSimulationGlobal" "engineOn" "eyeDirection" "faction" "fleeing" "flyInHeight" "flyInHeightASL" "forceSpeed" "freeLook" "fuel" "fullCrew" "getAllHitPointsDamage" "getAllUnitTraits" "getAttackTarget" "getBleedingRemaining" "getBurningValue" "getCameraViewDirection" "getCargoIndex" "getCenterOfMass" "getCruiseControl" "getDammage" "getDescription" "getDir" "getDirVisual" "getDiverState" "getHideFrom" "getHit" "getHitIndex" "getHitPointDamage" "getMass" "getModelInfo" "getObjectFOV" "getObjectMaterials" "getObjectScale" "getObjectTextures" "getObjectType" "getOpticsMode" "getOxygenRemaining" "getPlateNumber" "getPosASLVisual" "getPosATLVisual" "getPosVisual" "getPosWorldVisual" "getRelDir" "getSpeed" "getSuppression" "getUnitFreefallInfo" "getUnitTrait" "getVehicleTIPars" "gunner" "handsHit" "hideBehindScripted" "hideBody" "hideObject" "hideObjectGlobal" "hideSelection" "incapacitatedState" "inflame" "inflamed" "isAbleToBreathe" "isAllowedCrewInImmobile" "isAutoHoverOn" "isAwake" "isBleeding" "isBurning" "isCopilotEnabled" "isDamageAllowed" "isEngineOn" "isFlashlightOn" "isHidden" "isHideBehindScripted" "isInRemainsCollector" "isKindOf" "isMarkedForCollection" "isObjectHidden" "isSimpleObject" "isTouchingGround" "isTurnedOut" "isWalking" "knowsAbout" "lifeState" "limitSpeed" "lock" "lockCargo" "lockDriver" "locked" "lockedCargo" "lockedDriver" "modelToWorldVisual" "modelToWorldVisualWorld" "morale" "namedProperties" "nameSound" "objectParent" "periscopeElevation" "player" "precision" "preloadObject" "rating" "removeFromRemainsCollector" "reveal" "roleDescription" "scudState" "selectionNames" "selectionPosition" "selectionVectorDirAndUp" "selectNoPlayer" "selectPlayer" "setAirplaneThrottle" "setBleedingRemaining" "setCaptive" "setCenterOfMass" "setConvoySeparation" "setCruiseControl" "setDamage" "setDammage" "setDir" "setDriveOnPath" "setFeatureType" "setFlagOwner" "setFuel" "setHit" "setHitIndex" "setHitPointDamage" "setMass" "setMimic" "setMissileTarget" "setMissileTargetPos" "setObjectMaterial" "setObjectMaterialGlobal" "setObjectScale" "setObjectTexture" "setObjectTextureGlobal" "setOpticsMode" "setOxygenRemaining" "setPlateNumber" "setRandomLip" "setSuppression" "setTargetAge" "setTowParent" "setUnconscious" "setUnitFreefallHeight" "setUnitRecoilCoefficient" "setUnitTrait" "setVehicleArmor" "setVehicleId" "setVehicleInit" "setVehicleLock" "setVehiclePosition" "setVehicleTIPars" "setVehicleVarName" "setVelocity" "setVelocityModelSpace" "setVelocityTransformation" "simulationEnabled" "sizeOf" "speed" "squadParams" "synchronizedObjects" "synchronizeObjectsAdd" "synchronizeObjectsRemove" "targetKnowledge" "targetsAggregate" "triggerAmmo" "typeOf" "unassignVehicle" "underwater" "unitAimPosition" "unitAimPositionVisual" "unitRecoilCoefficient" "vehicle" "vehicleMoveInfo" "vehicleVarName" "velocity" "velocityModelSpace" "weaponLowered" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Particles
(defconst arma-commands-particles
    (rx (or bol space "," "[" "{" "(")
        (group (or "drop" "particlesQuality" "setDropInterval" "setParticleCircle" "setParticleClass" "setParticleFire" "setParticleParams" "setParticleRandom" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Pilot_Camera
(defconst arma-commands-pilot-camera
    (rx (or bol space "," "[" "{" "(")
        (group (or "getPilotCameraDirection" "getPilotCameraPosition" "getPilotCameraRotation" "getPilotCameraTarget" "hasPilotCamera" "setPilotCameraDirection" "setPilotCameraRotation" "setPilotCameraTarget" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Positions
(defconst arma-commands-positions
    (rx (or bol space "," "[" "{" "(")
        (group (or "AGLToASL" "aimPos" "ASLToAGL" "ASLToATL" "ATLToASL" "buildingPos" "eyePos" "findEmptyPosition" "findEmptyPositionReady" "formationPosition" "getPos" "getPosASL" "getPosASLW" "getPosATL" "getPosWorld" "getRelPos" "getTerrainHeightASL" "inArea" "inAreaArray" "inPolygon" "isFlatEmpty" "mapGridPosition" "modelToWorld" "modelToWorldWorld" "position" "positionCameraToWorld" "posScreenToWorld" "posWorldToScreen" "screenToWorld" "setPos" "setPosASL" "setPosASLW" "setPosATL" "setPosWorld" "setVehiclePosition" "surfaceIsWater" "surfaceNormal" "surfaceTexture" "surfaceType" "unitAimPosition" "unitsBelowHeight" "visiblePosition" "visiblePositionASL" "worldToModel" "worldToModelVisual" "worldToScreen" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Program_Flow
(defconst arma-commands-program-flow
    (rx (or bol space "," "[" "{" "(")
        (group (or "assert" "break" "breakOut" "breakTo" "breakWith" "call" "canSuspend" "case" "catch" "continue" "continueWith" "default" "do" "else" "exec" "execFSM" "execVM" "exit" "exitWith" "fileExists" "for" "forEach" "forEachMember" "forEachMemberAgent" "forEachMemberTeam" "from" "goto" "halt" "if" "isUIContext" "loadFile" "scopeName" "scriptDone" "scriptName" "sleep" "spawn" "step" "switch" "terminate" "then" "throw" "to" "try" "uiSleep" "waitUntil" "while" "with" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Radio_and_Chat
(defconst arma-commands-radio-and-chat
    (rx (or bol space "," "[" "{" "(")
        (group (or "channelEnabled" "clearRadio" "commandChat" "commandRadio" "conversationDisabled" "currentChannel" "customChat" "customRadio" "directSay" "disableConversation" "enableChannel" "enableRadio" "enableSentences" "getPlayerChannel" "getPlayerVoNVolume" "getSubtitleOptions" "globalChat" "globalRadio" "groupChat" "groupRadio" "radioChannelAdd" "radioChannelCreate" "radioChannelInfo" "radioChannelRemove" "radioChannelSetCallSign" "radioChannelSetLabel" "radioEnabled" "sentencesEnabled" "setCurrentChannel" "setPlayerVoNVolume" "showChat" "shownChat" "shownRadio" "shownSubtitles" "showRadio" "showSubtitles" "sideChat" "sideRadio" "systemChat" "vehicleChat" "vehicleRadio" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Regular_Expression_(Regex)
(defconst arma-commands-regular-expression-regex
    (rx (or bol space "," "[" "{" "(")
        (group (or "regexFind" "regexMatch" "regexReplace" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Remote_Control
(defconst arma-commands-remote-control
    (rx (or bol space "," "[" "{" "(")
        (group (or "allUnitsUAV" "connectTerminalToUAV" "disableUAVConnectability" "enableUAVConnectability" "enableUAVWaypoints" "getConnectedUAV" "getConnectedUAVUnit" "isAutonomous" "isUAVConnectable" "isUAVConnected" "remoteControl" "setAutonomous" "shownUAVFeed" "showUAVFeed" "UAVControl" "unitIsUAV" "waypointsEnabledUAV" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Render_Time_Scope
(defconst arma-commands-render-time-scope
    (rx (or bol space "," "[" "{" "(")
        (group (or "getPosVisual" "visiblePosition" "getPos" "position" "getPosATLVisual" "getPosATL" "getPosASLVisual" "visiblePositionASL" "getPosASL" "getPosASLW" "AGLToASL" "modelToWorldVisual" "getPosWorld" "modelToWorldVisual" "modelToWorld" "worldToModelVisual" "worldToModel" "getDirVisual" "getPosASLVisual" "getPosATLVisual" "getPosVisual" "getPosWorldVisual" "modelToWorldVisual" "modelToWorldVisualWorld" "selectionPosition" "unitAimPositionVisual" "vectorDirVisual" "vectorModelToWorldVisual" "vectorUpVisual" "vectorWorldToModelVisual" "visiblePosition" "visiblePositionASL" "worldToModelVisual" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Roads_and_Airports
(defconst arma-commands-roads-and-airports
    (rx (or bol space "," "[" "{" "(")
        (group (or "airportSide" "allAirports" "assignToAirport" "getRoadInfo" "isOnRoad" "landAt" "nearRoads" "roadAt" "roadsConnectedTo" "setAirportSide" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Ropes_and_Sling_Loading
(defconst arma-commands-ropes-and-sling-loading
    (rx (or bol space "," "[" "{" "(")
        (group (or "canSlingLoad" "enableRopeAttach" "getSlingLoad" "ropeAttachedObjects" "ropeAttachedTo" "ropeAttachEnabled" "ropeAttachTo" "ropeCreate" "ropeCut" "ropeDestroy" "ropeDetach" "ropeEndPosition" "ropeLength" "ropes" "ropesAttachedTo" "ropeSegments" "ropeSetCargoMass" "ropeUnwind" "ropeUnwound" "setSlingLoad" "setTowParent" "slingLoadAssistantShown" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_RTD
(defconst arma-commands-rtd
    (rx (or bol space "," "[" "{" "(")
        (group (or "difficultyEnabledRTD" "addForceGeneratorRTD" "airDensityCurveRTD" "airDensityRTD" "batteryChargeRTD" "clearForcesRTD" "collectiveRTD" "difficultyEnabledRTD" "enableAutoStartUpRTD" "enableAutoTrimRTD" "enableStressDamage" "enginesIsOnRTD" "enginesPowerRTD" "enginesRpmRTD" "enginesTorqueRTD" "forceAtPositionRTD" "forceGeneratorRTD" "getEngineTargetRPMRTD" "getRotorBrakeRTD" "getTrimOffsetRTD" "getWingsOrientationRTD" "getWingsPositionRTD" "isAutoStartUpEnabledRTD" "isAutoTrimOnRTD" "isObjectRTD" "isStressDamageEnabled" "numberOfEnginesRTD" "rotorsForcesRTD" "rotorsRpmRTD" "setActualCollectiveRTD" "setAPURTD" "setBatteryChargeRTD" "setBatteryRTD" "setBrakesRTD" "setCustomWeightRTD" "setEngineRpmRTD" "setForceGeneratorRTD" "setRotorBrakeRTD" "setStarterRTD" "setThrottleRTD" "setWantedRPMRTD" "setWingForceScaleRTD" "stopEngineRTD" "throttleRTD" "weightRTD" "windRTD" "wingsForcesRTD" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Sensors
(defconst arma-commands-sensors
    (rx (or bol space "," "[" "{" "(")
        (group (or "confirmSensorTarget" "enableVehicleSensor" "getRemoteSensorsDisabled" "getSensorTargets" "getSensorThreats" "isSensorTargetConfirmed" "isVehicleRadarOn" "isVehicleSensorEnabled" "listRemoteTargets" "listVehicleSensors" "reportRemoteTarget" "setVehicleRadar" "setVehicleReceiveRemoteTargets" "setVehicleReportOwnPosition" "setVehicleReportRemoteTargets" "vehicleReceiveRemoteTargets" "vehicleReportOwnPosition" "vehicleReportRemoteTargets" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Sides
(defconst arma-commands-sides
    (rx (or bol space "," "[" "{" "(")
        (group (or "addScoreSide" "airportSide" "allSites" "blufor" "captive" "captiveNum" "civilian" "countEnemy" "countFriendly" "countSide" "countUnknown" "createCenter" "createSite" "deleteCenter" "east" "enemy" "faction" "friendly" "getFriend" "independent" "knowsAbout" "opfor" "playerSide" "resistance" "scoreSide" "setAirportSide" "setCaptive" "setFriend" "setSide" "side" "sideAmbientLife" "sideEmpty" "sideEnemy" "sideFriendly" "sideLogic" "sideUnknown" "west" "WFSideText" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Sounds
(defconst arma-commands-sounds
    (rx (or bol space "," "[" "{" "(")
        (group (or "addMusicEventHandler" "commandChat" "commandRadio" "createSoundSource" "customRadio" "directSay" "enableAudioFeature" "enableEnvironment" "enableRadio" "enableSentences" "environmentEnabled" "environmentVolume" "fadeEnvironment" "fadeMusic" "fadeRadio" "fadeSound" "fadeSpeech" "getAllEnvSoundControllers" "getAllSoundControllers" "getAudioOptionVolumes" "getCustomSoundController" "getCustomSoundControllerCount" "getEnvSoundController" "getMusicPlayedTime" "getSoundController" "getSoundControllerResult" "globalRadio" "groupRadio" "musicVolume" "onBriefingGear" "onBriefingGroup" "onBriefingNotes" "onBriefingPlan" "onBriefingTeamSwitch" "pitch" "playMusic" "playSound" "playSoundUI" "preloadSound" "radioVolume" "removeAllMusicEventHandlers" "removeMusicEventHandler" "say" "setCustomSoundController" "setMusicEffect" "setMusicEventHandler" "setPitch" "setSoundEffect" "setSpeaker" "sideRadio" "soundVolume" "speaker" "speechVolume" "vehicleRadio" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Stamina_System
(defconst arma-commands-stamina-system
    (rx (or bol space "," "[" "{" "(")
        (group (or "allowSprint" "enableAimPrecision" "enableFatigue" "enableStamina" "forceWalk" "getAimingCoef" "getAnimAimPrecision" "getAnimSpeedCoef" "getCustomAimCoef" "getFatigue" "getStamina" "getWeaponSway" "isAimPrecisionEnabled" "isForcedWalk" "isSprintAllowed" "isStaminaEnabled" "setAnimSpeedCoef" "setCustomAimCoef" "setFatigue" "setStamina" "setStaminaScheme" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Strings
(defconst arma-commands-strings
    (rx (or bol space "," "[" "{" "(")
        (group (or "comment" "compile" "compileFinal" "composeText" "copyFromClipboard" "copyToClipboard" "count" "endl" "find" "forceUnicode" "format" "formatText" "getTextWidth" "hint" "hintC" "hintSilent" "image" "in" "insert" "isLocalized" "joinString" "lineBreak" "localize" "parseNumber" "parseSimpleArray" "parseText" "select" "setAttributes" "splitString" "str" "text" "toArray" "toFixed" "toLower" "toLowerANSI" "toString" "toUpper" "toUpperANSI" "trim" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Structured_Text
(defconst arma-commands-structured-text
    (rx (or bol space "," "[" "{" "(")
        (group (or "composeText" "ctrlSetStructuredText" "formatText" "hint" "hintC" "hintSilent" "image" "lineBreak" "parseText" "setAttributes" "text" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_System
(defconst arma-commands-system
    (rx (or bol space "," "[" "{" "(")
        (group (or "addLiveStats" "armoryPoints" "benchmark" "calculatePlayerVisibilityByFriendly" "callExtension" "cheatsEnabled" "compileScript" "completedFSM" "connectToServer" "copyFromClipboard" "copyToClipboard" "currentNamespace" "distributionRegion" "enableCaustics" "enableSatNormalOnDetail" "endLoadingScreen" "exportLandscapeXYZ" "finishMissionInit" "getCalculatePlayerVisibilityByFriendly" "getFieldManualStartPage" "getMissionPath" "getObjectViewDistance" "getPiPViewDistance" "getResolution" "getShadowDistance" "getStatValue" "getSteamFriendsServers" "getTerrainGrid" "getTextureInfo" "hierarchyObjectsCount" "isAutotest" "isFilePatchingEnabled" "isGameFocused" "isGamePaused" "isPiPEnabled" "isStreamFriendlyUIEnabled" "keyImage" "keyName" "language" "libraryCredits" "libraryDisclaimers" "openDSInterface" "openSteamApp" "openYoutubeVideo" "playMission" "playScriptedMission" "preprocessFile" "preprocessFileLineNumbers" "processInitCommands" "productVersion" "profileName" "profileNameSteam" "progressLoadingScreen" "requiredVersion" "reversedMouseY" "runInitScript" "saveJoysticks" "screenshot" "setArmoryPoints" "setDetailMapBlendPars" "setFSMVariable" "setHorizonParallaxCoef" "setObjectViewDistance" "setPiPViewDistance" "setShadowDistance" "setStatValue" "setSystemOfUnits" "setTerrainGrid" "setToneMapping" "setToneMappingParams" "setViewDistance" "squadParams" "startLoadingScreen" "supportInfo" "systemOfUnits" "unlockAchievement" "useAIOperMapObstructionTest" "viewDistance" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Team_Switch
(defconst arma-commands-team-switch
    (rx (or bol space "," "[" "{" "(")
        (group (or "Enlarge" "addSwitchableUnit" "enableTeamSwitch" "onBriefingTeamSwitch" "onTeamSwitch" "removeSwitchableUnit" "switchableUnits" "teamSwitch" "teamSwitchEnabled" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Teams
(defconst arma-commands-teams
    (rx (or bol space "," "[" "{" "(")
        (group (or "Team" "addResources" "addTeamMember" "agent" "agents" "createTask" "createTeam" "currentTasks" "deleteResources" "deleteTeam" "isAgent" "members" "registeredTasks" "registerTask" "removeTeamMember" "resources" "sendTask" "setLeader" "teamMember" "teamMemberNull" "teamName" "teams" "teamType" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Time
(defconst arma-commands-time
    (rx (or bol space "," "[" "{" "(")
        (group (or "accTime" "dayTime" "estimatedEndServerTime" "estimatedTimeLeft" "serverTime" "setAccTime" "setTimeMultiplier" "skipTime" "systemTime" "systemTimeUTC" "time" "timeMultiplier" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Triggers
(defconst arma-commands-triggers
    (rx (or bol space "," "[" "{" "(")
        (group (or "Triggers" "createTrigger" "inArea" "inAreaArray" "list" "setEffectCondition" "setMusicEffect" "setRadioMsg" "setSoundEffect" "setTitleEffect" "setTriggerActivation" "setTriggerArea" "setTriggerInterval" "setTriggerStatements" "setTriggerText" "setTriggerTimeout" "setTriggerType" "synchronizedTriggers" "synchronizedWaypoints" "synchronizeTrigger" "synchronizeWaypoint" "triggerActivated" "triggerActivation" "triggerArea" "triggerAttachedVehicle" "triggerAttachObject" "triggerAttachVehicle" "triggerInterval" "triggerStatements" "triggerText" "triggerTimeout" "triggerTimeoutCurrent" "triggerType" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Turrets
(defconst arma-commands-turrets
    (rx (or bol space "," "[" "{" "(")
        (group (or "addMagazineTurret" "addWeaponTurret" "allTurrets" "assignAsTurret" "currentMagazineDetailTurret" "currentMagazineTurret" "currentWeaponTurret" "directionStabilizationEnabled" "enableDirectionStabilization" "enablePersonTurret" "getTurretOpticsMode" "loadMagazine" "lockedTurret" "lockTurret" "magazinesAllTurrets" "magazinesTurret" "magazineTurretAmmo" "moveInTurret" "removeMagazinesTurret" "removeMagazineTurret" "removeWeaponTurret" "selectWeaponTurret" "setMagazineTurretAmmo" "setTurretOpticsMode" "turretLocal" "turretOwner" "turretUnit" "unitTurret" "weaponsTurret" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Uncategorised
(defconst arma-commands-uncategorised
    (rx (or bol space "," "[" "{" "(")
        (group (or  ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Unit_Control
(defconst arma-commands-unit-control
    (rx (or bol space "," "[" "{" "(")
        (group (or "allowGetIn" "assignAsCargo" "assignAsCargoIndex" "assignAsCommander" "assignAsDriver" "assignAsGunner" "assignedCargo" "assignedCommander" "assignedDriver" "assignedGunner" "attackEnabled" "commandFire" "commandFollow" "commandFSM" "commandGetOut" "commandSuppressiveFire" "commandWatch" "currentCommand" "doFire" "doFollow" "doFSM" "doGetOut" "doMove" "doStop" "doSuppressiveFire" "doTarget" "doWatch" "effectiveCommander" "emptyPositions" "enableAttack" "expectedDestination" "fire" "fireAtTarget" "forceFollowRoad" "forceWeaponFire" "forgetTarget" "formationLeader" "formLeader" "getCruiseControl" "glanceAt" "hideBehindScripted" "in" "land" "landAt" "landResult" "leaveVehicle" "limitSpeed" "lookAt" "move" "moveInAny" "moveInCargo" "moveInCommander" "moveInDriver" "moveInGunner" "moveOut" "moveTo" "moveToCompleted" "moveToFailed" "orderGetIn" "playAction" "pose" "sendSimpleCommand" "setCruiseControl" "setDestination" "setEffectiveCommander" "setFormationTask" "setUnitPos" "setUnitPosWeak" "setUnloadInCombat" "stance" "stop" "stopped" "suppressFor" "swimInDepth" "switchAction" "unassignVehicle" "unitPos" "unitReady" "vehicleMoveInfo" "weaponDirection" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Unit_Identity
(defconst arma-commands-unit-identity
    (rx (or bol space "," "[" "{" "(")
        (group (or "deleteIdentity" "face" "loadIdentity" "lockIdentity" "name" "pitch" "rank" "rankId" "saveIdentity" "setFace" "setIdentity" "setMimic" "setName" "setNameSound" "setPitch" "setRank" "setSpeaker" "setUnitRank" "speaker" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Unit_Inventory
(defconst arma-commands-unit-inventory
    (rx (or bol space "," "[" "{" "(")
        (group (or "addBackpackGlobal" "addBinocularItem" "addGoggles" "addHandgunItem" "addHeadgear" "addItem" "addItemToUniform" "addItemToVest" "addMagazine" "addMagazineGlobal" "addMagazines" "addPrimaryWeaponItem" "addSecondaryWeaponItem" "addUniform" "addVest" "addWeapon" "addWeaponGlobal" "addWeaponItem" "ammo" "assignedItems" "assignItem" "backpackMagazines" "binocular" "binocularItems" "binocularMagazine" "canAdd" "canAddItemToUniform" "canAddItemToVest" "currentMagazine" "currentMagazineDetail" "currentThrowable" "currentWeapon" "forceAddUniform" "gearIDCAmmoCount" "gearSlotAmmoCount" "gearSlotData" "getContainerMaxLoad" "getUnitLoadout" "goggles" "handgunItems" "handgunMagazine" "handgunWeapon" "hasWeapon" "headgear" "hmd" "isUniformAllowed" "items" "itemsWithMagazines" "linkItem" "load" "loadAbs" "loadUniform" "loadVest" "lockedInventory" "lockInventory" "magazines" "magazinesAmmo" "magazinesAmmoFull" "magazinesDetail" "magazinesDetailBackpack" "magazinesDetailUniform" "magazinesDetailVest" "maxLoad" "primaryWeapon" "primaryWeaponItems" "primaryWeaponMagazine" "removeAllAssignedItems" "removeAllBinocularItems" "removeAllContainers" "removeAllHandgunItems" "removeAllItems" "removeAllItemsWithMagazines" "removeAllPrimaryWeaponItems" "removeAllSecondaryWeaponItems" "removeAllWeapons" "removeBackpackGlobal" "removeBinocularItem" "removeClothing" "removeGoggles" "removeHandgunItem" "removeHeadgear" "removeItem" "removeItemFromUniform" "removeItemFromVest" "removeItems" "removeMagazine" "removeMagazineGlobal" "removeMagazines" "removePrimaryWeaponItem" "removeSecondaryWeaponItem" "removeUniform" "removeVest" "removeWeapon" "removeWeaponGlobal" "secondaryWeapon" "secondaryWeaponItems" "secondaryWeaponMagazine" "setAmmo" "setMaxLoad" "setUnitLoadout" "setVehicleAmmoDef" "soldierMagazines" "someAmmo" "unassignItem" "uniform" "uniformContainer" "uniformItems" "uniformMagazines" "uniqueUnitItems" "unlinkItem" "vest" "vestContainer" "vestItems" "vestMagazines" "weaponAccessories" "weaponAccessoriesCargo" "weaponCargo" "weapons" "weaponsItems" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Variables
(defconst arma-commands-variables
    (rx (or bol space "," "[" "{" "(")
        (group (or "addPublicVariableEventHandler" "allVariables" "and" "configNull" "diaryRecordNull" "displayNull" "false" "getFSMVariable" "getVariable" "isEqualTo" "isEqualType" "isEqualTypeAll" "isEqualTypeAny" "isEqualTypeArray" "isEqualTypeParams" "isFinal" "isMissionProfileNamespaceLoaded" "isNil" "isNotEqualTo" "isNull" "localNamespace" "locationNull" "missionNamespace" "missionProfileNamespace" "netObjNull" "nil" "not" "objNull" "or" "param" "params" "parsingNamespace" "private" "profileNamespace" "publicVariable" "publicVariableClient" "publicVariableServer" "saveMissionProfileNamespace" "saveProfileNamespace" "saveVar" "scriptNull" "serverNamespace" "setVariable" "taskNull" "true" "typeName" "uiNamespace" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Vehicle_in_Vehicle_Transport
(defconst arma-commands-vehicle-in-vehicle-transport
    (rx (or bol space "," "[" "{" "(")
        (group (or "canVehicleCargo" "enableVehicleCargo" "getVehicleCargo" "isVehicleCargo" "setVehicleCargo" "vehicleCargoEnabled" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Vehicle_Inventory
(defconst arma-commands-vehicle-inventory
    (rx (or bol space "," "[" "{" "(")
        (group (or "getFuelCargo" "addItemCargo" "addItemCargoGlobal" "addMagazineAmmoCargo" "addWeapon" "addWeaponGlobal" "addWeaponWithAttachmentsCargo" "addWeaponWithAttachmentsCargoGlobal" "currentMagazine" "currentMagazineDetail" "everyContainer" "getAmmoCargo" "getFuelCargo" "getItemCargo" "getMagazineCargo" "getRepairCargo" "getWeaponCargo" "itemCargo" "magazineCargo" "magazines" "magazinesAmmo" "magazinesAmmoCargo" "magazinesAmmoFull" "magazinesDetail" "removeWeapon" "removeWeaponAttachmentCargo" "removeWeaponCargo" "removeWeaponGlobal" "setAmmo" "setAmmoCargo" "setFuelCargo" "setRepairCargo" "setVehicleAmmo" "setVehicleAmmoDef" "weaponAccessoriesCargo" "weaponCargo" "weapons" "weaponsItems" "weaponsItemsCargo" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Vehicle_Loadouts
(defconst arma-commands-vehicle-loadouts
    (rx (or bol space "," "[" "{" "(")
        (group (or "ammoOnPylon" "animateBay" "animatePylon" "getAllPylonsInfo" "getCompatiblePylonMagazines" "getPylonMagazines" "setAmmoOnPylon" "setPylonLoadout" "setPylonsPriority" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Waypoints
(defconst arma-commands-waypoints
    (rx (or bol space "," "[" "{" "(")
        (group (or "Waypoints" "addWaypoint" "copyWaypoints" "createGuardedPoint" "currentWaypoint" "customWaypointPosition" "deleteWaypoint" "enableUAVWaypoints" "getWPPos" "lockWP" "setCurrentWaypoint" "setMusicEffect" "setSoundEffect" "setTitleEffect" "setWaypointBehaviour" "setWaypointCombatMode" "setWaypointCompletionRadius" "setWaypointDescription" "setWaypointForceBehaviour" "setWaypointFormation" "setWaypointHousePosition" "setWaypointLoiterAltitude" "setWaypointLoiterRadius" "setWaypointLoiterType" "setWaypointName" "setWaypointPosition" "setWaypointScript" "setWaypointSpeed" "setWaypointStatements" "setWaypointTimeout" "setWaypointType" "setWaypointVisible" "setWPPos" "showWaypoint" "showWaypoints" "synchronizedWaypoints" "synchronizeTrigger" "synchronizeWaypoint" "waypointAttachedObject" "waypointAttachedVehicle" "waypointAttachObject" "waypointAttachVehicle" "waypointBehaviour" "waypointCombatMode" "waypointCompletionRadius" "waypointDescription" "waypointForceBehaviour" "waypointFormation" "waypointHousePosition" "waypointLoiterAltitude" "waypointLoiterRadius" "waypointLoiterType" "waypointName" "waypointPosition" "waypoints" "waypointScript" "waypointsEnabledUAV" "waypointShow" "waypointSpeed" "waypointStatements" "waypointTimeout" "waypointTimeoutCurrent" "waypointType" "waypointVisible" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Weapon_Pool
(defconst arma-commands-weapon-pool
    (rx (or bol space "," "[" "{" "(")
        (group (or "addItemPool" "addMagazinePool" "addWeaponPool" "clearItemPool" "clearMagazinePool" "clearWeaponPool" "fillWeaponsFromPool" "loadStatus" "pickWeaponPool" "putWeaponPool" "queryItemsPool" "queryMagazinePool" "queryWeaponPool" "saveStatus" ))
    (or space "," "]" "}" ")" ";" )))

;; Parsing scripting commands from https://community.bistudio.com/wiki/Category:Command_Group:_Weapons
(defconst arma-commands-weapons
    (rx (or bol space "," "[" "{" "(")
        (group (or "aimedAtTarget" "canDeployWeapon" "canFire" "compatibleItems" "compatibleMagazines" "currentMuzzle" "currentThrowable" "currentVisionMode" "currentWeapon" "currentWeaponMode" "currentZeroing" "disableNVGEquipment" "disableTIEquipment" "enableGunLights" "enableIRLasers" "enableReload" "enableWeaponDisassembly" "fire" "fireAtTarget" "forceWeaponFire" "getShotParents" "getWeaponSway" "hasWeapon" "isFlashlightOn" "isLaserOn" "isManualFire" "isWeaponDeployed" "isWeaponRested" "laserTarget" "loadMagazine" "missileTarget" "missileTargetPos" "needReload" "reload" "reloadEnabled" "selectWeapon" "setMissileTarget" "setMissileTargetPos" "setShotParents" "setWeaponReloadingTime" "setWeaponZeroing" "weaponDirection" "weaponInertia" "weaponLowered" "weaponReloadingTime" "weaponsInfo" "weaponState" ))
    (or space "," "]" "}" ")" ";" )))

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
    (,arma-commands-animations 1 font-lock-builtin-face)
    (,arma-commands-arrays 1 font-lock-builtin-face)
    (,arma-commands-artillery 1 font-lock-builtin-face)
    (,arma-commands-briefing 1 font-lock-builtin-face)
    (,arma-commands-broken-commands 1 font-lock-builtin-face)
    (,arma-commands-camera-control 1 font-lock-builtin-face)
    (,arma-commands-config 1 font-lock-builtin-face)
    (,arma-commands-containers 1 font-lock-builtin-face)
    (,arma-commands-conversations 1 font-lock-builtin-face)
    (,arma-commands-curator 1 font-lock-builtin-face)
    (,arma-commands-custom-panels 1 font-lock-builtin-face)
    (,arma-commands-diagnostic 1 font-lock-builtin-face)
    (,arma-commands-difficulty 1 font-lock-builtin-face)
    (,arma-commands-dynamic-simulation 1 font-lock-builtin-face)
    (,arma-commands-eden-editor 1 font-lock-builtin-face)
    (,arma-commands-environment 1 font-lock-builtin-face)
    (,arma-commands-event-handlers 1 font-lock-builtin-face)
    (,arma-commands-flags 1 font-lock-builtin-face)
    (,arma-commands-game- 1 font-lock-builtin-face)
    (,arma-commands-groups 1 font-lock-builtin-face)
    (,arma-commands-gui-control 1 font-lock-builtin-face)
    (,arma-commands-hashmap 1 font-lock-builtin-face)
    (,arma-commands-high-command 1 font-lock-builtin-face)
    (,arma-commands-interaction 1 font-lock-builtin-face)
    (,arma-commands-leaderboards 1 font-lock-builtin-face)
    (,arma-commands-lights 1 font-lock-builtin-face)
    (,arma-commands-localization 1 font-lock-builtin-face)
    (,arma-commands-locations 1 font-lock-builtin-face)
    (,arma-commands-map 1 font-lock-builtin-face)
    (,arma-commands-markers 1 font-lock-builtin-face)
    (,arma-commands-math 1 font-lock-builtin-face)
    (,arma-commands-mines 1 font-lock-builtin-face)
    (,arma-commands-mission-information 1 font-lock-builtin-face)
    (,arma-commands-mods-and-addons 1 font-lock-builtin-face)
    (,arma-commands-multiplayer 1 font-lock-builtin-face)
    (,arma-commands-object-detection 1 font-lock-builtin-face)
    (,arma-commands-object-manipulation 1 font-lock-builtin-face)
    (,arma-commands-particles 1 font-lock-builtin-face)
    (,arma-commands-pilot-camera 1 font-lock-builtin-face)
    (,arma-commands-positions 1 font-lock-builtin-face)
    (,arma-commands-program-flow 1 font-lock-builtin-face)
    (,arma-commands-radio-and-chat 1 font-lock-builtin-face)
    (,arma-commands-regular-expression-regex 1 font-lock-builtin-face)
    (,arma-commands-remote-control 1 font-lock-builtin-face)
    (,arma-commands-render-time-scope 1 font-lock-builtin-face)
    (,arma-commands-roads-and-airports 1 font-lock-builtin-face)
    (,arma-commands-ropes-and-sling-loading 1 font-lock-builtin-face)
    (,arma-commands-rtd 1 font-lock-builtin-face)
    (,arma-commands-sensors 1 font-lock-builtin-face)
    (,arma-commands-sides 1 font-lock-builtin-face)
    (,arma-commands-sounds 1 font-lock-builtin-face)
    (,arma-commands-stamina-system 1 font-lock-builtin-face)
    (,arma-commands-strings 1 font-lock-builtin-face)
    (,arma-commands-structured-text 1 font-lock-builtin-face)
    (,arma-commands-system 1 font-lock-builtin-face)
    (,arma-commands-team-switch 1 font-lock-builtin-face)
    (,arma-commands-teams 1 font-lock-builtin-face)
    (,arma-commands-time 1 font-lock-builtin-face)
    (,arma-commands-triggers 1 font-lock-builtin-face)
    (,arma-commands-turrets 1 font-lock-builtin-face)
    (,arma-commands-uncategorised 1 font-lock-builtin-face)
    (,arma-commands-unit-control 1 font-lock-builtin-face)
    (,arma-commands-unit-identity 1 font-lock-builtin-face)
    (,arma-commands-unit-inventory 1 font-lock-builtin-face)
    (,arma-commands-variables 1 font-lock-builtin-face)
    (,arma-commands-vehicle-in-vehicle-transport 1 font-lock-builtin-face)
    (,arma-commands-vehicle-inventory 1 font-lock-builtin-face)
    (,arma-commands-vehicle-loadouts 1 font-lock-builtin-face)
    (,arma-commands-waypoints 1 font-lock-builtin-face)
    (,arma-commands-weapon-pool 1 font-lock-builtin-face)
    (,arma-commands-weapons 1 font-lock-builtin-face)
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

