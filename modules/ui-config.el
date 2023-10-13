;;; ui-config.el --- Init file -*- lexical-binding: t -*-
(use-package doom-themes
  :config
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
  :init
    (load-theme 'doom-peacock t)
    (disable-theme 'deeper-blue)
    (enable-theme 'doom-peacock)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Diminish the modeline
(use-package diminish)

;; ---------------
;; Display Options
;; ===============

;; Show line numbers
(global-display-line-numbers-mode 1)

;; Even when narrowing, show global line numbers
(setq-default display-line-numbers-widen t)

;; Show the column as well as the line
(setq column-number-mode t)

;; Show matching parentheses
(show-paren-mode 1)

;; Don't blink, it's too distracting.
(setq blink-matching-paren nil)
(setq show-paren-delay 0.2)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)

;; Disable word-wrap. Why?
;; .. confusing for reading structured text, where white-space can be significant.
(setq-default truncate-lines t)

;; Disable GUI elements. Why?
;; .. they take up screen-space and are unnecessary, favor a minimal interface.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable startup message. Why?
;; .. less noise is better.
(defun display-startup-echo-area-message () (message ""))
;; Window Title, include the buffer name & modified status. Why?
;; .. the buffer name helps to differentiate windows when selecting from a task list.
(setq-default frame-title-format "%b %& emacs")
;; Visual bell. Why?
;; .. audible beeps are annoying.
(setq visible-bell 1)
;; Show text instead prompts instead of dialog popups. Why?
;; .. because they're not as nice for quick keyboard access.
(setq use-dialog-box nil)
;; For text-mode prompts. Why?
;; .. answering just 'y' or 'n' is sufficient.
(defalias 'yes-or-no-p 'y-or-n-p)
;; Show empty lines. Why?
;; .. without this you can't tell if there are blank lines at the end of the file.
(setq-default indicate-empty-lines t)
;; Keep cursors and highlights in current window only. Why?
;; .. it's not especially useful to show these in inactive windows.
(setq cursor-in-non-selected-windows 'hollow)
;; Highlight inactive windows. Why?
;; .. to keep the selection region when changing windows (when evil-mode is disabled).
(setq highlight-nonselected-windows t)
;; Don't show buffer list on startup. Why?
;; .. buffer switching gets in the way, you can manually switch between them.
(setq inhibit-startup-buffer-menu t)
;; Hide mouse cursor while typing. Why?
;; .. it can overlap characters we want to see.
(setq make-pointer-invisible t)
;; Don't put two spaces after full-stop. Why?
;; .. one space after a full-stop is sufficient in most documentation & comments.
(setq sentence-end-double-space nil)

;; Highlight terms in code-comments such as TODO, FIXME, URL's & email. Why?
;; .. these are common conventions in software that it's useful to highlight them.
(use-package hl-prog-extra
  :commands (hl-prog-extra-mode)
  :hook ((prog-mode) . hl-prog-extra-mode))

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
             default-font-presets-scale-reset))

(use-package git-gutter-fringe
  :init
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(defun ml/pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                   scroll-down-command
                   evil-window-left
                   evil-window-right
                   evil-window-up
                   evil-window-down
                   recenter-top-bottom
                   other-window))
  (advice-add command :after #'ml/pulse-line))

(provide 'ui-config)
;;; ui-config.el ends here
