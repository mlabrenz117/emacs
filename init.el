;;; init.el --- Init file -*- lexical-binding: t -*-

;; ----------------------------------------------------------------------------
;; Application Options
;; ###################

;; Disable GUI elements. Why?
;; .. they take up screen-space and are unnecessary, favor a minimal interface.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable startup message. Why?
;; .. less noise is better.
(defun display-startup-echo-area-message () (message ""))

;; Visual bell. Why?
;; .. audible beeps are annoying.
(setq visible-bell 1)

;; Window Title, include the buffer name & modified status. Why?
;; .. the buffer name helps to differentiate windows when selecting from a task list.
(setq-default frame-title-format "%b %& emacs")

;; ----------------------------------------------------------------------------
;; Defaults
;; ########

;; Use UTF-8 everywhere. Why?
;; .. this is the most common encoding, saves hassles guessing and getting it wrong.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Show text instead prompts instead of dialog popups. Why?
;; .. because they're not as nice for quick keyboard access.
(setq use-dialog-box nil)

;; For text-mode prompts. Why?
;; .. answering just 'y' or 'n' is sufficient.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Store registers on exit. Why?
;; .. nice to keep macros available on restart.
(savehist-mode 1)
(setq savehist-additional-variables '(register-alist))

;; Don't use file backups. Why?
;; .. it adds cruft on the file-system which gets annoying.
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Show empty lines. Why?
;; .. without this you can't tell if there are blank lines at the end of the file.
(setq-default indicate-empty-lines t)

;; Keep cursors and highlights in current window only. Why?
;; .. it's not especially useful to show these in inactive windows.
(setq cursor-in-non-selected-windows 'hollow)
;; Highlight inactive windows. Why?
;; .. to keep the selection region when changing windows (when evil-mode is disabled).
(setq highlight-nonselected-windows t)
;; Disable bidirectional text support. Why?
;; .. slight performance improvement.
(setq bidi-display-reordering nil)

;; No startup screen. Why?
;; .. no need to distract us with unnecessary info.
(setq inhibit-startup-screen t)

;; Don't show buffer list on startup. Why?
;; .. buffer switching gets in the way, you can manually switch between them.
(setq inhibit-startup-buffer-menu t)

;; Hide mouse cursor while typing. Why?
;; .. it can overlap characters we want to see.
(setq make-pointer-invisible t)

;; Don't put two spaces after full-stop. Why?
;; .. one space after a full-stop is sufficient in most documentation & comments.
(setq sentence-end-double-space nil)

;; ---------
;; Scrolling
;; =========

;; Scroll N lines to screen edge. Why?
;; .. having some margin is useful to see some lines above/below the lines you edit.
(setq scroll-margin 8)

;; Scroll back this many lines to being the cursor back on screen. Why?
;; .. default behavior is to re-center which is jarring. Clamp to the scroll margin instead.
(setq scroll-conservatively scroll-margin)

;; Keyboard scroll one line at a time. Why?
;; .. having scrolling jump is jarring & unnecessary (use page up down in this case).
(setq scroll-step 1)
;; Mouse scroll N lines. Why?
;; .. speed is fast but slower than page up/down (a little personal preference).
(setq mouse-wheel-scroll-amount '(6 ((shift) . 1)))
;; Don't accelerate scrolling. Why?
;; .. it makes scrolling distance unpredictable.
(setq mouse-wheel-progressive-speed nil)
;; Don't use timer when scrolling. Why?
;; .. it's not especially useful, one less timer for a little less overhead.
(setq mouse-wheel-inhibit-click-time nil)

;; Preserve line/column (nicer page up/down). Why?
;; .. avoids having the cursor at the top/bottom edges.
(setq scroll-preserve-screen-position t)
;; Move the cursor to top/bottom even if the screen is viewing top/bottom (for page up/down). Why?
;; .. so pressing page/up down can move the cursor & the view to start/end of the buffer.
(setq scroll-error-top-bottom t)

;; Center after going to the next compiler error. Why?
;; .. don't get stuck at screen edges.
(setq next-error-recenter (quote (4)))

;; Always redraw immediately when scrolling. Why?
;; .. more responsive, it wont hang while handling keyboard input.
(setq fast-but-imprecise-scrolling nil)
(setq jit-lock-defer-time 0)

;; -----------------
;; Clipboard Support
;; =================

;; Cutting & pasting use the system clipboard. Why?
;; .. integrates with the system clipboard for convenience.
(setq select-enable-clipboard t)

;; Treat clipboard input as UTF-8 string first; compound text next, etc. Why?
;; .. match default encoding which is UTF-8 as well.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Paste at text-cursor instead of mouse-cursor location. Why?
;; .. allow to quickly select & paste while in insert-mode, instead of moving the text cursor.
(setq mouse-yank-at-point t)

;; ----------------------------------------------------------------------------
;; Editing Options
;; ###############

;; Undo
;; ====

;; Don't group undo steps. Why?
;; .. without this is groups actions into a fixed number of steps which feels unpredictable.
(fset 'undo-auto-amalgamate 'ignore)

;; Increase undo limits. Why?
;; .. ability to go far back in history can be useful, modern systems have sufficient memory.
;; Limit of 64mb.
(setq undo-limit 6710886400)
;; Strong limit of 1.5x (96mb)
(setq undo-strong-limit 100663296)
;; Outer limit of 10x (960mb).
;; Note that the default is x100), but this seems too high.
(setq undo-outer-limit 1006632960)


;; Case Sensitivity
;; ================

;; Be case sensitive. Why?
;; .. less ambiguous results, most programming languages are case sensitive.

;; Case sensitive search.
(setq-default case-fold-search nil)
;; Case sensitive abbreviations.
(setq dabbrev-case-fold-search nil)
;; Case sensitive (impacts counsel case-sensitive file search).
(setq-default search-upper-case nil)


;; -----------
;; Indentation
;; ===========

;; yes, both are needed!
(setq default-tab-width 4)
(setq tab-width 4)
(setq default-fill-column 80)
(setq fill-column 80)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default evil-shift-round t)

;; Auto close pairs
(electric-pair-mode t)
(recentf-mode)
(global-auto-revert-mode)


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
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This is only needed once, near the top of the file
(eval-when-compile (require 'use-package))

(use-package diminish
  :ensure t
  :demand t)

;; Download automatically. Why?
;; .. convenience, so on first start all packages are installed.
(setq use-package-always-ensure t)
;; Defer loading packages by default. Why?
;; .. faster startup for packages which are only activated on certain modes or key bindings.
(setq use-package-always-defer t)

;; Add the ability to upgrade all packages. Why?
;; .. adds a quick way to upgrade everything at once.
(use-package package-utils
  :commands (package-utils-upgrade-all-and-recompile))

;; Nice theme from Vim. Why?
;; .. personal preference.
;; (use-package inkpot-theme
;;   :demand t
;;   :config (load-theme 'inkpot t t))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :init
  (load-theme 'doom-peacock t)
  (enable-theme 'doom-peacock)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; (use-package gruvbox-theme
  ;; :demand t)
  ;; :config (load-theme 'gruvbox-dark-hard t))

(use-package slime
  :config (setq inferior-lisp-program "sbcl"))

;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (load-theme 'ample-light t t)
;;                (enable-theme 'ample)
;;                (with-eval-after-load "ample-theme"
;;                  (custom-theme-set-faces
;;                   'ample
;;                   '(font-lock-string-face ((t (:foreground "#057f40"))))))))


;; Highlight terms in code-comments such as TODO, FIXME, URL's & email. Why?
;; .. these are common conventions in software that it's useful to highlight them.
(use-package hl-prog-extra
  :commands (hl-prog-extra-mode)
  :init (add-hook 'prog-mode-hook #'hl-prog-extra-mode))


;; ---------
;; Terminal
;; ---------

(use-package vterm
  :config
  (setq vterm-shell "/usr/bin/nu")
  (add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode 0))))

;; ---------
;; Git
;; ---------
(use-package magit
  :config
  (add-hook 'magit-mode-hook
            (lambda ()
              (define-key magit-mode-map (kbd "C-j") #'magit-status-quick)
              (define-key magit-mode-map (kbd "j") #'magit-next-line)
              (define-key magit-mode-map (kbd "C-k") #'magit-delete-thing)
              (define-key magit-mode-map (kbd "k") #'magit-previous-line)
              (define-key magit-status-mode-map (kbd "C-j") #'magit-status-jump)
              (define-key magit-status-mode-map (kbd "j") #'magit-next-line)
              (define-key magit-log-mode-map (kbd "C-j") #'magit-log-move-to-revision)
              (define-key magit-log-mode-map (kbd "j") #'magit-next-line)
              (define-key magit-diff-mode-map (kbd "C-j") #'magit-jump-to-diffstat-or-diff)
              (define-key magit-diff-mode-map (kbd "j") #'magit-next-line))))

;; Main Vim emulation package. Why?
;; .. without this, you won't have Vim key bindings or modes.
(use-package evil
  :demand t
  :init
  ;; See `undo-fu' package.
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-i-jump nil)
  ;; For some reasons evils own search isn't default.
  (setq evil-search-module 'evil-search)
  :config
  ;; Initialize.
  (evil-mode)

  ;; (add-hook 'magit-mode-hook
  ;;   (lambda ()
  ;;     (dolist (mode '(magit-mode-map magit-blob-mode-map magit-log-mode magit-wip-mode magit-diff-mode magit-refs-mode magit-stash-mode magit-cherry-mode magit-reflog-mode magit-status-mode magit-process-mode magit-selection-mode magit-stashes-mode magit-repolist-mode magit-reversion-mode magit-log-select-mode magit-auto-revert-mode magit-merge-preview-mode magit-submodule-list-mode magit-wip-after-save-mode magit-blame-read-only-mode magit-wip-after-apply-mode magit-wip-before-change-mode magit-wip-initial-backup-mode))
  ;;       (evil-define-key 'emacs mode
  ;;         "C-j" magit-next-line
  ;;         "C-k" magit-previous-line
  ;;         "/" 'evil-search-forward
  ;;         ))))
  ;; (add-hook 'magit-mode-hook
  ;;   (lambda ()
  ;;     (evil-add-hjkl-bindings magit-mode-map 'emacs
  ;;       (kbd "/")    'evil-search-forward
  ;;       (kbd "n")    'evil-search-next
  ;;       (kbd "N")    'evil-search-previous
  ;;       (kbd "C-d")  'evil-scroll-down
  ;;       (kbd "C-u")  'evil-scroll-up
  ;;       (kbd "C-w C-w")  'other-window)))

  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'occur-edit-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'insert)

  (add-hook 'occur-mode-hook
    (lambda ()
      (evil-add-hjkl-bindings occur-mode-map 'emacs)
      (evil-add-hjkl-bindings occur-edit-mode-map 'emacs
        (kbd "/")    'evil-search-forward
        (kbd "n")    'evil-search-next
        (kbd "N")    'evil-search-previous
        (kbd "C-d")  'evil-scroll-down
        (kbd "C-u")  'evil-scroll-up
        (kbd "i")    'occur-edit-mode
        (kbd "C-w C-w")  'other-window)))

  (setq evil-ex-search-case 'sensitive))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :after org
  :hook (org-mode . (lambda () evil-bullets-mode 1)))

(use-package evil-commentary
  :demand t
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :demand t
  :config
  (global-evil-surround-mode))

(use-package evil-indent-plus
  :demand t
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-numbers)

;; Use a thin wrapper for undo. Why?
;; .. By default undo doesn't support redo as most users would expect from other software.
(use-package undo-fu)

;; Prompt for available keys after some delay. Why?
;; .. useful to see available keys after some delay, especially for evil-leader key.
(use-package which-key
  :demand t
  :config
  ;; Initialize.
  (which-key-mode))

;; Enable vertico
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(setq corfu-auto t
      corfu-quit-no-match 'separator)
(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :init
  (global-corfu-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.

  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/documents/roam"))
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-autosync-mode t))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :demand t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Highlights numbers. Why?
;; .. Emacs doesn't do this by default, use a package.
(use-package highlight-numbers
  :hook ((prog-mode) . highlight-numbers-mode))

(use-package rainbow-delimiters
  :hook ((proj-mode) . rainbow-delimiters-mode))

;; Scale all text. Why?
;; .. it's useful sometimes to globally zoom in all text.
(use-package default-font-presets
  :commands (default-font-presets-scale-increase
             default-font-presets-scale-decrease
             default-font-presets-scale-reset)
  :demand t)

;; ---------------
;; Display Options
;; ===============

;; Show line numbers. Why?
;; Helpful to give context when reading errors & the current line is made more prominent.
(global-display-line-numbers-mode 1)

;; Even when narrowing, show global line numbers. Why?
;; .. because these are often referenced in external messages.
(setq-default display-line-numbers-widen t)

;; Show the column as well as the line. Why?
;; .. some compiler errors show the column which is useful to compare.
(setq column-number-mode t)

;; Show matching parentheses. Why?
;; .. handy for developers to match nested brackets.
(show-paren-mode 1)
;; Don't blink, it's too distracting.
(setq blink-matching-paren nil)
(setq show-paren-delay 0.2)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)

;; Disable word-wrap. Why?
;; .. confusing for reading structured text, where white-space can be significant.
(setq-default truncate-lines t)

;; ------------
;; File Formats
;; ============

;; Options for generic modes. Why?
;; .. this avoids duplicating checks for all programming and text modes.
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   (cond
    ((derived-mode-p 'prog-mode)
     (flyspell-prog-mode))
    ((derived-mode-p 'text-mode)
     (flyspell-mode)))))


(add-hook
 'after-change-major-mode-hook
 (lambda ()
   (cond
    ((derived-mode-p 'prog-mode)
     (whitespace-mode)))))

;; ------
;; Markup
;; ------

(add-hook 'org-mode-hook
  (lambda ()
    (setq-local fill-column 120)
    (setq-local tab-width 2)
    (setq-local evil-shift-width 2)
    (setq-local indent-tabs-mode nil)

    (setq-local ffip-patterns '("*.org"))))

;; ---------
;; Scripting
;; ---------

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq-local fill-column 120)
    (setq-local tab-width 2)
    (setq-local evil-shift-width 2)
    (setq-local indent-tabs-mode nil)

    (setq-local ffip-patterns '("*.el"))

    ;; Don't delimit on dashes or underscores. Why?
    ;; .. makes searching for variable names inconvenient.
    (modify-syntax-entry ?- "w")
    (modify-syntax-entry ?_ "w")))

;; -----
;; Shell
;; -----

(add-hook 'sh-mode-hook
  (lambda ()
    (setq-local fill-column 120)
    (setq-local tab-width 4)
    (setq-local evil-shift-width 4)
    (setq-local indent-tabs-mode nil)

    (setq-local ffip-patterns '("*.sh"))))

;; ---------------
;; Other Languages
;; ---------------

;; -----
;; Rust
;; -----
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-atuo-activate nil)

  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'ml/rustic-mode-hook))
(defun ml/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-diaplay-inline-hints nil)
  ;; (lsp-rust-analyzer-server-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package consult-lsp)

;; -----
;;   C
;; -----

(add-hook 'c-mode-hook
  (lambda ()
    (setq-local fill-column 120)
    (setq-local c-basic-offset 4)
    (setq-local tab-width 4)
    (setq-local evil-shift-width 4)
    (setq-local indent-tabs-mode nil)

    (setq-local ffip-patterns
      '("*.c" "*.cc" "*.cpp" "*.cxx" "*.h" "*.hh" "*.hpp" "*.hxx" "*.inl"))

    ;; Don't delimit on '_'. Why?
    ;; .. makes searching for variable names inconvenient.
    (modify-syntax-entry ?_ "w")))

;; ------
;; Python
;; ------

(add-hook 'python-mode-hook
  (lambda ()
    (setq-local fill-column 80)
    (setq-local tab-width 4)
    (setq-local evil-shift-width 4)
    (setq-local indent-tabs-mode nil)

    (setq-local ffip-patterns '("*.py"))))

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


;; --------------
;; Evil Mode Keys
;; ==============

;; Use secondary selection in insert mode, Why?
;; .. this is handy for quick middle mouse copy/paste while in insert mode.
(define-key evil-insert-state-map (kbd "<down-mouse-1>") 'mouse-drag-secondary)
(define-key evil-insert-state-map (kbd "<drag-mouse-1>") 'mouse-drag-secondary)
(define-key evil-insert-state-map (kbd "<mouse-1>") 'mouse-start-secondary)
;; De-select after copy, Why?
;; .. allows quick select-copy-paste.
(define-key evil-insert-state-map (kbd "<mouse-2>")
  (lambda (click)
    (interactive "*p")
    (when (overlay-start mouse-secondary-overlay)
      (mouse-yank-secondary click)
      (delete-overlay mouse-secondary-overlay))))

;; Vim increment/decrement keys.
(define-key evil-normal-state-map (kbd "<leader>a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "<leader>x") 'evil-numbers/dec-at-pt)

(define-key evil-visual-state-map (kbd "g <leader>a") 'evil-numbers/inc-at-pt-incremental)
(define-key evil-visual-state-map (kbd "g <leader>x") 'evil-numbers/dec-at-pt-incremental)

(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-n") 'evil-insert-digraph)

;; ----------------
;; Evil Leader Keys
;; ================

;; Example leader keys for useful functionality exposed by packages.
(with-eval-after-load 'evil
  (evil-set-leader '(normal) (kbd ","))

  ;; Interactive file name search.

  ;; Interactive file content search (git).
  ;; (evil-define-key 'normal 'global (kbd "<leader>f") 'consult-git-grep)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>r") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>t") 'vterm)
  ;; Interactive current-file search.
  (evil-define-key 'normal 'global (kbd "<leader>s") 'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader>S") 'consult-line-multi)
  ;; Interactive open-buffer switch.
  (evil-define-key 'normal 'global (kbd "<leader>b") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>v") 'consult-project-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>k") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>g") 'magit))


;; ----------------------------------------------------------------------------
;; Custom Variables
;; ################

;; Store custom variables in an external file. Why?
;; .. it means this file can be kept in version control without noise from custom variables.

;; (set-fontset-font "fontset-standard
;; (set-font-face 'default "fontset-standard")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
