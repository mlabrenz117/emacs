;;; register-packages.el --- Init file -*- lexical-binding: t -*-

;; ----------------------------------------------------------------------------
;; Packages
;; ########

(require 'package)
(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if
                                            ; not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions
                                            ; from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it
                                            ; from melpa

;; Auto-install use-package. Why?
;; .. this is a defacto-standard package manager, useful to isolate each package's configuration.
(when (and (version< emacs-version "29")
           (not (package-installed-p 'use-package)))
    (package-refresh-contents)
    (package-install 'use-package))

;; This is only needed once, near the top of the file
(eval-when-compile (require 'use-package))

;; Download automatically. Why?
;; .. convenience, so on first start all packages are installed.
;;(setq use-package-always-ensure nil)

;; Defer loading packages by default. Why?
;; .. faster startup for packages which are only activated on certain modes or key bindings.
(setq use-package-always-defer t)

(dolist (package (list
                  ;;; Core Packages
                  'evil
                  'evil-org
                  'evil-commentary
                  'evil-surround
                  'evil-indent-plus
                  'evil-numbers
                  'magit
                  'vterm
                  ;;; UI Packages
                  'doom-themes
                  'doom-modeline
                  'diminish
                  'hl-prog-extra
                  'highlight-numbers
                  'rainbow-delimiters
                  'default-font-presets
                  ;;Minibuffer/Completion Packages
                  'vertico
                  'consult
                  'orderless
                  'corfu
                  'marginalia
                  'embark
                  'embark-consult
                  'which-key
                  ;;; Org Packages
                  'org-roam
                  'org-bullets
                  ;;; Language Packages
                  'rustic
                  'slime
                  ;;; Utility Packages
                  'package-utils
                  'emacsql-sqlite-builtin
                  'undo-fu))
  (add-to-list 'package-selected-packages package))

(package-install-selected-packages t)

(provide 'register-packages)
;;; register-packages.el ends here
