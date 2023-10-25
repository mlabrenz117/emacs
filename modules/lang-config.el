;;; lang-config.el --- Init file -*- lexical-binding: t -*-

;; ------------
;; File Formats
;; ============

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

;; ;; Options for generic modes. Why?
;; ;; .. this avoids duplicating checks for all programming and text modes.
;; (add-hook
;;  'after-change-major-mode-hook
;;  (lambda ()
;;    (cond
;;     ((derived-mode-p 'prog-mode)
;;      (flyspell-prog-mode))
;;     ((derived-mode-p 'text-mode)
;;      (flyspell-mode)))))


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

;; -----------
;; Common Lisp
;; -----------

(use-package slime
  :config (setq inferior-lisp-program "sbcl"))

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

;; ----------
;; FYT Config
;; ----------

(defun fyt/accept-checkin-prompt (process string)
  "Accept checkin prompt during magit git checkin"
  (when-let ((beg (string-match "Is this list correct?" string)))
    (process-send-string
     process
     "Y\n")))
(add-hook 'magit-process-prompt-functions 'fyt/accept-checkin-prompt)

(defun fyt/search-codebase ()
  "Grep for a string in the FYT codebase using `rg'."
  (interactive)
  (consult-ripgrep
   `("~/workspace/palms.fileyourtaxes.com/common"
     "~/workspace/palms.fileyourtaxes.com/staticFiles"
     "~/workspace/palms.fileyourtaxes.com/accountServices"
     "~/workspace/palms.fileyourtaxes.com/prepTool"
     "~/workspace/palms.fileyourtaxes.com/crm"
     "~/workspace/palms.fileyourtaxes.com/taxCenter"
     "~/workspace/palms.fileyourtaxes.com/transmissions"
     "~/workspace/palms.fileyourtaxes.com/IND23"
     "~/workspace/palms.fileyourtaxes.com/ET23") ""))

(defun fyt/search-backend ()
  "Grep for a string in the FYT codebase using `rg'."
  (interactive)
  (consult-ripgrep
   `("~/workspace/palms.fileyourtaxes.com/common"
     "~/workspace/palms.fileyourtaxes.com/staticFiles"
     "~/workspace/palms.fileyourtaxes.com/accountServices"
     "~/workspace/palms.fileyourtaxes.com/prepTool"
     "~/workspace/palms.fileyourtaxes.com/crm"
     "~/workspace/palms.fileyourtaxes.com/taxCenter"
     "~/workspace/palms.fileyourtaxes.com/transmissions") ""))

(provide 'lang-config)
;;; lang-config.el --- ends here
