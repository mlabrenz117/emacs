;;; init.el --- Init file -*- lexical-binding: t -*-

;; ----------------------------------------------------------------------------
;; Application Options
;; ###################

;; Adds modules to the `load-path' and sets up a basic config
(load (expand-file-name "modules/default-config" user-emacs-directory))

(require 'register-packages)

(require 'core-config)

(require 'ui-config)

(require 'org-config)

(require 'minibuffer-completion-config)

(require 'lang-config)

;; ;; Add the ability to upgrade all packages. Why?
;; ;; .. adds a quick way to upgrade everything at once.
(use-package package-utils
  :commands (package-utils-upgrade-all-and-recompile))

;; ----------------------------------------------------------------------------
;; Key-map
;; #######

;; -----------
;; Global Keys
;; ===========

;; Control +/- or mouse-wheel to zoom. Why?
;; .. this is a common shortcut for web-browsers that doesn't conflict with anything else.

(global-set-key (kbd "C-=") 'default-font-presets-scale-increase)
(global-set-key (kbd "C--") 'default-font-presets-scale-decrease)
(global-set-key (kbd "C-0") 'default-font-presets-scale-reset)

(global-set-key (kbd "<C-mouse-4>") 'default-font-presets-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'default-font-presets-scale-decrease)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)



;; ----------------------------------------------------------------------------
;; Custom Variables
;; ################

;; Store custom variables in an external file. Why?
;; .. it means this file can be kept in version control without noise from custom variables.

;; (set-fontset-font "fontset-standard
;; (set-font-face 'default "fontset-standard")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
