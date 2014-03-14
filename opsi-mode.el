;;; opsi-mode-el -- Major mode for editing Winst/OPSI files

;; Author: Daniel Koch <koch@triple6.org>
;; Created: 07 Mar 2014
;; Keywords: OPSI major-mode
;; Version: 0.7
;; Description:
;; This is a Major-Mode for Editing Winst/OPSI files
;; as involved in the software deployment software OPSI by uib
;; See opsi.org for details

;; Copyright (C) 2014 Daniel Koch <koch@triple6.org>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(defvar opsi-mode-hook nil)
(defvar opsi-mode-map
  (let ((opsi-mode-map (make-keymap)))
;    (define-key opsi-mode-map "\C-j" 'newline-and-indent)
    opsi-mode-map)
  "Keymap for OPSI major mode")


;; ,----
;; | File-Extensions
;; `----
(add-to-list 'auto-mode-alist '("\\.opsi.*\\'" . opsi-mode))
(add-to-list 'auto-mode-alist '("\\.ins\\'" . opsi-mode))

;; ,----
;; | Syntax
;; `----
(setq opsi-parts '(
"Actions" "actions"
"Initial" "initial"
"ProfileActions" "Profileactions" "profileactions"
))

(setq opsi-sections '(
"DosBatch" "Dosbatch" "dosbatch"
"DosInAnIcon" "Dosinanicon" "dosinanicon"
"ExecPython" "Execpython" "execpython"
"ExecWith" "Execwith" "execwith"
"Files" "files"
"LdapSearch" "Ldapsearch" "ldapsearch"
"LinkFolder" "Linkfolder" "linkfolder"
"OpsiServiceCall" "Opsiservicecall" "opsiservicecall"
"PatchHosts" "Patchhosts" "patchhosts"
"PatchTextFile" "Patchtextfile" "patchtextfile"
"Patches" "patches"
"Registry" "registry"
"Sub" "sub"
"WinBatch" "Winbatch" "winbatch"
"XMLPatch" "XMLpatch" "xmlpatch"
))

(setq opsi-constants '(
"AllUsersProfileDir" "Allusersprofiledir" "allusersprofiledir"
"AppdataDir" "Appdatadir" "appdatadir"
"CommonAppdataDir" "Commonappdatadir" "commonappdatadir"
"CommonDesktopDir" "Commondesktopdir" "commondesktopdir"
"CommonProfileDir" "Commonprofiledir" "commonprofiledir"
"CommonProgramsDir" "Commonprogramsdir" "commonprogramsdir"
"CommonStartMenuPath" "Commonstartmenupath" "commonstartmenupath"
"CommonStartmenuDir" "Commonstartmenudir" "commonstartmenudir"
"CommonStartupDir" "Commonstartupdir" "commonstartupdir"
"CurrentAppdataDir" "Currentappdatadir" "currentappdatadir"
"CurrentDesktopDir" "Currentdesktopdir" "currentdesktopdir"
"CurrentProfileDir" "Currentprofiledir" "currentprofiledir"
"CurrentProgramsDir" "Currentprogramsdir" "currentprogramsdir"
"CurrentSendToDir" "Currentsendtodir" "currentsendtodir"
"CurrentStartmenuDir" "Currentstartmenudir" "currentstartmenudir"
"CurrentStartupDir" "Currentstartupdir" "currentstartupdir"
"DefaultUserProfileDir" "Defaultuserprofiledir" "defaultuserprofiledir"
"Host" "host"
"HostID" "Hostid" "hostid"
"IPAddress" "Ipaddress" "ipaddress"
"InstallingProdName" "installingProdName" "Installingprodname" "installingprodname"
"InstallingProdVersion" "installingProdVersion" "Installingprodversion" "installingprodversion"
"InstallingProduct" "installingProduct" "Installingproduct" "installingproduct"
"LogFile" "Logfile" "logfile"
"OpsiScriptHelperPath" "opsiScriptHelperPath" "Opsiscripthelperpath" "opsiscripthelperpath"
"OpsiServicePassword" "opsiServicePassword" "opsiservicePassword" "Opsiservicepassword" "opsiservicepassword"
"OpsiServiceURL" "opsiServiceURL" "opsiserviceURL" "Opsiserviceurl" "opsiserviceurl"
"OpsiServiceUser" "opsiServiceUser" "opsiserviceUser" "Opsiserviceuser" "opsiserviceuser"
"PCName" "Pcname" "pcname"
"ProfileDir" "Profiledir" "profiledir"
"ProgramFiles32Dir" "Programfiles32dir" "programfiles32dir"
"ProgramFiles64Dir" "Programfiles64dir" "programfiles64dir"
"ProgramFilesDir" "Programfilesdir" "programfilesdir"
"ProgramFilesSysnativeDir" "Programfilessysnativedir" "programfilessysnativedir"
"ScriptDir" "Scriptdir" "scriptdir"
"ScriptDrive" "Scriptdrive" "scriptdrive"
"ScriptPath" "Scriptpath" "scriptpath"
"System" "system"
"SystemRoot" "Systemroot" "systemroot"
"UserProfileDir" "Userprofiledir" "userprofiledir"
"Username" "username"
"WinstDir" "Winstdir" "winstdir"
"WinstVersion" "Winstversion" "winstversion"
"opsiServer" "Opsiserver" "opsiserver"


))

(setq opsi-functions '(
"="
"AND" "And" "and"
"ChangeDirectory" "Changedirectory" "changedirectory"
"Comment" "comment"
"CompareDotSeparatedNumbers" "Comparedotseparatednumbers" "comparedotseparatednumbers"
"CompareDotSeparatedStrings" "Comparedotseparatedstrings" "comparedotseparatedstrings"
"DecStrToHexStr" "Decstrtohexstr" "decstrtohexstr"
"DefStringList" "Defstringlist" "defstringlist"
"DefVar" "Defvar" "defvar"
"Else" "else"
"EndIf" "Endif" "endif"
"EnvVar" "Envvar" "envvar"
"ExitOnError" "Exitonerror" "exitonerror"
"ExitWindows" "ExitWindows" "Exitwindows" "exitwindows"
"ExtractFilePath" "Extractfilepath" "extractfilepath"
"FatalOnSyntaxError" "Fatalonsyntaxerror" "fatalonsyntaxerror"
"FileExists" "Fileexists" "fileexists"
"FileExists32" "Fileexists32" "fileexists32"
"FileExists64" "Fileexists64" "fileexists64"
"FileExistsSysNative" "Fileexistssysnative" "fileexistssysnative"
"For" "for"
"GetHostsAddr" "Gethostsaddr" "gethostsaddr"
"GetHostsName" "Gethostsname" "gethostsname"
"GetIpByName" "Getipbyname" "getipbyname"
"GetLocaleInfoMap" "Getlocaleinfomap" "getlocaleinfomap"
"GetLoggedInUser" "Getloggedinuser" "getloggedinuser"
"GetMSVersionMap" "Getmsversionmap" "getmsversionmap"
"GetMsVersionInfo" "getMsVersionInfo" "Getmsversioninfo" "getmsversioninfo"
"GetMyIpByTarget" "Getmyipbytarget" "getmyipbytarget"
"GetNtVersion" "Getntversion" "getntversion"
"GetOS" "Getos" "getos"
"GetProcessList" "Getprocesslist" "getprocesslist"
"GetProductProperty" "getProductProperty" "Getproductproperty" "getproductproperty"
"GetRegistryStringValue32" "Getregistrystringvalue32" "getregistrystringvalue32"
"GetRegistryStringValue64" "Getregistrystringvalue64" "getregistrystringvalue64"
"GetRegistryStringValueSysNative" "Getregistrystringvaluesysnative" "getregistrystringvaluesysnative"
"GetRegistrystringvalue" "Getregistrystringvalue" "getregistrystringvalue"
"GetScriptMode" "Getscriptmode" "getscriptmode"
"GetSystemType" "GetSystemType" "Getsystemtype" "getsystemtype"
"GetUserSID" "Getusersid" "getusersid"
"GetUsercontext" "Getusercontext" "getusercontext"
"GetvalueFromInifile" "Getvaluefrominifile" "getvaluefrominifile"
"HasMinimumSpace" "Hasminimumspace" "hasminimumspace"
"HexStrToDecStr" "Hexstrtodecstr" "hexstrtodecstr"
"IconizeWinst" "Iconizewinst" "iconizewinst"
"If" "if"
"In" "in"
"Include_Append" "Include_Append" "Include_append"
"Include_Insert" "Include_Insert" "Include_insert"
"IniVar" "Inivar" "inivar"
"IsFatalError" "Isfatalerror" "isfatalerror"
"KillTask" "Killtask" "killtask"
"LineBeginning_Existsin" "linebeginning_ExistsIn" "LineBeginning_ExistsIn"
"LineExistsIn" "Lineexistsin" "lineexistsin"
"LogError" "logerror" "Logerror"
"LogWarning" "Logwarning" "logwarning"
"Message" "message"
"NOT" "Not" "not"
"NormalizeWinst" "Normalizewinst" "normalizewinst"
"OR" "Or" "or"
"ParamStr" "Paramstr" "paramstr"
"Pause" "pause"
"RandomStr" "Randomstr" "randomstr"
"RestoreWinst" "Restorewinst" "restorewinst"
"ScriptErrorMessages" "Scripterrormessages" "scripterrormessages"
"Set" "set"
"SetLogLevel" "Setloglevel" "setloglevel"
"SetSkinDirectory" "Setskindirectory" "setskindirectory"
"ShowBitmap" "Showbitmap" "showbitmap"
"ShowMessageFile" "Showmessagefile" "showmessagefile"
"SidToName" "Sidtoname" "sidtoname"
"SleepSeconds" "Sleepseconds" "sleepseconds"
"StayOnTop" "Stayontop" "stayontop"
"Stop" "stop"
"Sub" "sub"
"SubstringBefore" "Substringbefore" "substringbefore"
"TraceMode" "Tracemode" "tracemode"
"XMLAddNamespace" "Xmladdnamespace" "xmladdnamespace" "XMLaddnamespace"
"XMLRemoveNamespace" "Xmlremovenamespace" "xmlremovenamespace" "XMLremovenamespace"
"addListToList" "Addlisttolist" "addlisttolist"
"addtolist" "Addtolist" "addtolist"
"base64DecodeStr" "Base64decodestr" "base64decodestr" "Base64DecodeStr"
"base64EncodeStr" "Base64encodestr" "base64encodestr" "Base64EncodeStr"
"calculate" "Calculate"
"composeString" "Composestring" "composestring" "ComposeString"
"contains" "Contains"
"convert2Jsonstr" "Convert2jsonstr" "convert2jsonstr" "Convert2JsonStr"
"count" "Count"
"getDiffTimeSec" "Getdifftimesec" "getdifftimesec" "GetDiffTimeSec"
"getFileInfoMap" "Getfileinfomap" "getfileinfomap" "GetFileInfoMap"
"getLastExitCode" "Getlastexitcode" "getlastexitcode"
"getListContaining" "Getlistcontaining" "getlistcontaining" "GetListContaining"
"getOutStreamFromSection" "Getoutstreamfromsection" "getoutstreamfromsection" "GetOutStreamFromSection"
"getProductMap" "Getproductmap" "getproductmap" "GetProductMap"
"getProductPropertyList" "Getproductpropertylist" "getproductpropertylist" "GetProductPropertyList"
"getProfilesDirList" "Getprofilesdirlist" "getprofilesdirlist" "GetProfilesDirList"
"getRegistryKeyList32" "Getregistrykeylist32" "getregistrykeylist32" "GetRegistryKeyList32"
"getRegistryKeyList64" "Getregistrykeylist64" "getregistrykeylist64" "GetRegistryKeyList64"
"getRegistryKeyListSysNative" "Getregistrykeylistsysnative" "getregistrykeylistsysnative" "GetRegistryKeyListSysNative"
"getRegistryVarList32" "Getregistryvarlist32" "getregistryvarlist32" "GetRegistryVarList32"
"getRegistryVarList64" "Getregistryvarlist64" "getregistryvarlist64" "GetRegistryVarList64"
"getRegistryVarListSysNative" "Getregistryvarlistsysnative" "getregistryvarlistsysnative" "GetRegistryVarListSysNative"
"getRegistryVarMap32" "Getregistryvarmap32" "getregistryvarmap32" "GetRegistryVarMap32"
"getRegistryVarMap64" "Getregistryvarmap64" "getregistryvarmap64" "GetRegistryVarMap64"
"getRegistryVarMapSysNative" "Getregistryvarmapsysnative" "getregistryvarmapsysnative" "GetRegistryVarMapSysNative"
"getReturnListFromSection" "Getreturnlistfromsection" "getreturnlistfromsection" "GetReturnListFromSection"
"getSectionNames" "Getsectionnames" "getsectionnames" "GetSectionNames"
"getSubList" "Getsublist" "getsublist" "GetSubList"
"getValue" "Getvalue" "getvalue"
"getValueBySeparator" "Getvaluebyseparator" "getvaluebyseparator"
"includelog" "Includelog"
"isLoginScript" "Isloginscript" "isloginscript" "IsLoginScript"
"isNumber" "Isnumber" "isnumber" "IsNumber"
"loadTextFile" "Loadtextfile" "loadtextfile" "LoadTextFile"
"loadUnicodeTextFile" "Loadunicodetextfile" "loadunicodetextfile" "LoadUnicodeTextFile"
"lower" "Lower"
"opsiLicenseManagementEnabled" "Opsilicensemanagementenabled" "opsilicensemanagementenabled" "OpsiLicenseManagementEnabled"
"readVersionFromProfile" "Readversionfromprofile" "readversionfromprofile"
"requiredWinstVersion" "Requiredwinstversion" "requiredwinstversion" "RequiredWinstVersion"
"retrieveSection" "Retrievesection"
"reverse" "Reverse"
"runningAsAdmin" "Runningasadmin" "runningasadmin" "RunningAsAdmin"
"saveVersionToProfile" "Saveversiontoprofile" "saveversiontoprofile"
"scriptWasExecutedBefore" "Scriptwasexecutedbefore" "scriptwasexecutedbefore"
"splitString" "Splitstring" "splitstring" "SplitString"
"splitStringOnWhiteSpace" "Splitstringonwhitespace" "splitstringonwhitespace" "SplitStringOnWhitespace"
"strLength" "Strlength" "strlength"
"strPart" "Strpart" "strpart"
"strPos" "Strpos" "strpos"
"stringReplace" "Stringreplace" "stringreplace"
"takeFirstStringContaining" "Takefirststringcontaining" "takefirststringcontaining" "TakeFirstStringContaining"
"takeString" "Takestring" "takestring" "TakeString"
"trim" "Trim"
"unquote" "Unquote"
))

(setq opsi-functions-args '(
"/ImmediateReboot" "/Immediatereboot" "/immediatereboot"
"/Reboot" "/Reboot" "/reboot"
"/ShutdownWanted" "/Shutdownwanted" "/shutdownwanted"
"ImmediateLogout" "Immediatelogout" "immediatelogout"
"RebootWanted" "Rebootwanted" "rebootwanted"
"Timeout" "timeout"
"WaitForProcessEnding" "Waitforprocessending" "waitforprocessending"
))

;; []
(setq opsi-parts-regexp (concat "\\[" (regexp-opt opsi-parts ) "\\]"))

;; Section_
(setq opsi-sections-regexp (concat "\\(\\[\\|\\)" (regexp-opt opsi-sections ) "_\\(_\\|\\w\\)*\\(\\]\\|\\)"))

;; %
(setq opsi-constants-regexp (concat "%" (regexp-opt opsi-constants) "%"))

;; $
(setq opsi-variables-regexp "\\<\$\\(\\w\\|\_\\|\-\\)*\$\\>")

;; DefVar,Set
(setq opsi-functions-regexp (regexp-opt opsi-functions 'words))

;; Parsing ARGS with leading "/"
(setq opsi-functions-args-regexp (concat "\/" (regexp-opt opsi-functions-args)))


;;Warning ARGS. (Not legit)
(setq opsi-warning-args-regexp  "%\\w*%")

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq opsi-font-lock-keywords
  `(
    (,opsi-parts-regexp . font-lock-keyword-face)
    (,opsi-sections-regexp . font-lock-keyword-face)
    (,opsi-constants-regexp . font-lock-builtin-face)
    (,opsi-functions-regexp . font-lock-function-name-face)
    (,opsi-functions-args-regexp . font-lock-builtin-face)
    (,opsi-variables-regexp . font-lock-constant-face)
    (,opsi-warning-args-regexp . font-lock-warning-face)
    ;; note: order above matters. “opsi-keywords-regexp” goes last because
))

;;; Clear mem
(setq opsi-parts-regexp nil)
(setq opsi-sections-regexp nil)
(setq opsi-constants-regexp nil)
(setq opsi-functions-regexp nil)
(setq opsi-functions-args-regexp nil)
(setq opsi-variables-regexp nil)

(defvar opsi-mode-syntax-table
  (let ((opsi-mode-syntax-table (make-syntax-table )))

    ; Use ";" for commands
	(modify-syntax-entry ?\; "< b" opsi-mode-syntax-table) ; ";" Start a comment
	(modify-syntax-entry ?\n "> b" opsi-mode-syntax-table) ; "\n" Ends a comment
	(modify-syntax-entry ?\' "\""  opsi-mode-syntax-table) ; "'" qoutes a string
	(modify-syntax-entry ?\\ " "  opsi-mode-syntax-table)  ; "\" does nothing ( Escapes by default )



	opsi-mode-syntax-table)
  "Syntax table for opsi-mode")

;; ,----
;; | Comment-dwim
;; `----
(defun opsi-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
        (comment-start ";")
        )
    (comment-dwim arg)))


;; ,----
;; | Indentation
;; `----
(defun opsi-indent-line ()
  "Indent current line as OPSI code."
  (interactive)
  (beginning-of-line)
    (let ((not-indented t) cur-indent)
        (if (looking-at "^[ \t]*\\(Endif\\|Else\\)") ; If the line we are looking at is the end of a block, then decrease the indentation
	      (progn
		(save-excursion
		    (forward-line -1)
		      (setq cur-indent (- (current-indentation) tab-width)))
		(if (< cur-indent 0) ; We can't indent past the left margin
		    (setq cur-indent 0)))
	  (save-excursion
	      (while not-indented ; Iterate backwards until we find an indentation hint
		(forward-line -1)
		(if (looking-at "^[ \t]*Endif") ; This hint indicates that we need to indent at the level of the Endif token
		    (progn
			(setq cur-indent (current-indentation))
			  (setq not-indented nil))
		    (if (looking-at "^[ \t]*\\(If\\|Else\\)") ; This hint indicates that we need to indent an extra level
			(progn
			    (setq cur-indent (+ (current-indentation) tab-width)) ; Do the actual indenting
			    (setq not-indented nil))
		      (if (bobp)
			  (setq not-indented nil)
			))))))
	(if cur-indent
	    (indent-line-to cur-indent)
	  (indent-line-to 0))


)) ; If we didn't see an indentation hint, then allow no indentation



;; ,----
;; | Key Bindings
;; `----
(define-key opsi-mode-map (kbd "<backtab>") 'indent-relative)
(define-key opsi-mode-map [remap comment-dwim] 'opsi-comment-dwim)
(define-key opsi-mode-map (kbd "<f8>") 'opsi-makeproductfile)


;; ,----
;; | Functions
;; `----
(defun opsi-cd-proddir()
  (setq init-dir (concat(buffer-file-name)"/../"))
  (cd init-dir)
  (while (not(file-exists-p "OPSI/control"))
    (progn
      (message "Searching OPSI/control")
      (cd "..")))
  (message "Found OPSI/control"))

(defun opsi-find-control()
  (interactive)
  (opsi-cd-proddir)
  (find-file "OPSI/control"))

(defun opsi-find-any (arg)
  (opsi-cd-proddir)
  (opsi-get-files)
  (if (string= (concat opsi-file- arg) "")
  (message (concat "There is no "arg "Script")
  (find-file (concat "CLIENT_DATA/" opsi-file-setup )))))

(defun opsi-find-setup()
  (interactive)
  (opsi-cd-proddir)
  (opsi-get-files)
  (if (string= opsi-file-setup "")
  (message "There is no setupScript")
  (find-file (concat "CLIENT_DATA/" opsi-file-setup ))))

(defun opsi-find-uninstall()
  (interactive)
  (opsi-cd-proddir)
  (opsi-get-files)
  (if (string= opsi-file-uninstall "")
  (message "There is no uninstallScript")
  (find-file (concat "CLIENT_DATA/" opsi-file-uninstall ))))

(defun opsi-find-update()
  (interactive)
  (opsi-cd-proddir)
  (opsi-get-files)
  (if (string= opsi-file-update "")
  (message "There is no updateScript")
  (find-file (concat "CLIENT_DATA/" opsi-file-update ))))

(defun opsi-find-always()
  (interactive)
  (opsi-cd-proddir)
  (opsi-get-files)
  (if (string= opsi-file-always "")
  (message "There is no alwaysScript")
  (find-file (concat "CLIENT_DATA/" opsi-file-always ))))

(defun opsi-find-once()
  (interactive)
  (opsi-cd-proddir)
  (opsi-get-files)
  (if (string= opsi-file-once "")
  (message "There is no onceScript")
  (find-file (concat "CLIENT_DATA/" opsi-file-once ))))

(defun opsi-find-custom()
  (interactive)
  (opsi-cd-proddir)
  (opsi-get-files)
  (if (string= opsi-file-custom "")
  (message "There is no customScript")
  (find-file (concat "CLIENT_DATA/" opsi-file-custom ))))

(defun opsi-find-userlogin()
  (interactive)
  (opsi-cd-proddir)
  (opsi-get-files)
  (if (string= opsi-file-userlogin "")
  (message "There is no userloginScript")
  (find-file (concat "CLIENT_DATA/" opsi-file-userlogin ))))


(defun opsi-get-files ()
  (setq opsi-file-setup nil)
  (setq opsi-file-uninstall nil)
  (setq opsi-file-update nil)
  (setq opsi-file-always nil)
  (setq opsi-file-once nil)
  (setq opsi-file-custom nil)
  (setq opsi-file-userlogin nil)

  (setq opsi-file-setup
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep setupScript: OPSI/control|cut -d: -f2"))))

  (setq opsi-file-uninstall
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep uninstallScript: OPSI/control|cut -d: -f2"))))

(setq opsi-file-update
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep updateScript: OPSI/control|cut -d: -f2"))))

  (setq opsi-file-always
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep alwaysScript: OPSI/control|cut -d: -f2"))))

  (setq opsi-file-once
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep onceScript: OPSI/control|cut -d: -f2"))))

  (setq opsi-file-custom
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep customScript: OPSI/control|cut -d: -f2"))))

  (setq opsi-file-userlogin
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep userLoginScript: OPSI/control|cut -d: -f2"))))
)





(defun opsi-status ()
  "Test Only!"
  (interactive)
  (opsi-cd-proddir)
  (setq opsi-product-id
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep id: OPSI/control|cut -d: -f2"))))
  (setq opsi-product-minor-version
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep version: OPSI/control|cut -d: -f2|head -n 1"))))
  (setq opsi-product-major-version
	(replace-regexp-in-string "\n$" ""
				  (replace-regexp-in-string " " ""
							    (shell-command-to-string "grep version: OPSI/control|cut -d: -f2|tail -n 1"))))
  (message (concat "id:" opsi-product-id " major-version:" opsi-product-major-version " minor-version:" opsi-product-minor-version))
  (setq mode-name (concat "OPSI["opsi-product-id"_"opsi-product-major-version"-"opsi-product-minor-version"]"))

)

(defun opsi-major-update ()
  "Test Only!"
  (interactive)
  (opsi-cd-proddir)
  (opsi-status)
  (setq new-major-version nil)
  (setq new-major-version(read-no-blanks-input (concat "New Version:") opsi-product-major-version))
  (shell-command (concat "sed -i 's/version: " opsi-product-major-version "/version: " new-major-version "/' OPSI/control"))
  (opsi-cd-proddir)
  (opsi-status)
  (message (concat "New major-version is " opsi-product-major-version))
)

(defun opsi-minor-update ()
  "Test Only!"
  (interactive)
  (opsi-cd-proddir)
  (opsi-status)
  (setq new-minor-version nil)
  (setq new-minor-version(read-no-blanks-input (concat "New Version:") opsi-product-minor-version))
  (shell-command (concat "sed -i 's/version: " opsi-product-minor-version "/version: " new-minor-version "/' OPSI/control"))
  (opsi-cd-proddir)
  (opsi-status)
  (message (concat "New minor-version is " opsi-product-minor-version))
)

(defun opsi-makeproductfile ()
  "Test Only!"
  (interactive)
  (opsi-cd-proddir)
  (opsi-status)
  (if (y-or-n-p (concat "Change Version: " opsi-product-id "_" opsi-product-major-version "-" opsi-product-minor-version "?"))
      (progn
	(opsi-major-update)
	(opsi-minor-update)
	)
    )
  (if (y-or-n-p (concat "Make Product: " opsi-product-id "_" opsi-product-major-version "-" opsi-product-minor-version "?"))
      (shell-command (concat "opsi-makeproductfile -q" )
)
)
)

(defun opsi-install-package ()
  "Test Only!"
  (interactive)
  (opsi-cd-proddir)
  (opsi-status)
  (if (file-exists-p (concat opsi-product-id "_" opsi-product-major-version "-" opsi-product-minor-version ".opsi"))
      (if (y-or-n-p (concat "Install Package:" opsi-product-id "_" opsi-product-major-version "-" opsi-product-minor-version ".opsi?"))
	  (eshell-command (concat "opsi-package-manager -q -i " opsi-product-id "_" opsi-product-major-version "-" opsi-product-minor-version ".opsi && echo Installation of " opsi-product-id " completed without error" ))
	)
    )
  )

(defun opsi-install-package-on-all-depots ()
  "Test Only!"
  (interactive)
  (opsi-cd-proddir)
  (opsi-status)
  (if (file-exists-p (concat opsi-product-id "_" opsi-product-major-version "-" opsi-product-minor-version ".opsi"))
      (if (y-or-n-p (concat "Install Package on all depots:" opsi-product-id "_" opsi-product-major-version "-" opsi-product-minor-version ".opsi?"))
	  (eshell-command (concat "opsi-package-manager -q -dALL -i " opsi-product-id "_" opsi-product-major-version "-" opsi-product-minor-version ".opsi && echo Installation of " opsi-product-id " completed without error" ))
	)
    )
  )

;; ,----
;; | Mode Definition
;; `----

(defun opsi-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map opsi-mode-map)
  (set-syntax-table opsi-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(opsi-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'opsi-indent-line)
  (setq major-mode 'opsi-mode)
  (opsi-status)
  (run-hooks 'opsi-mode-hook))

(provide 'opsi-mode)
(provide 'opsi-mode-map)



;;; opsi-mode.el ends here
