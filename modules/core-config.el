;;; core-config.el -*- lexical-binding: t; -*-

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

  (evil-set-leader '(normal) (kbd ","))

  ;; Interactive file name search.

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
  (evil-define-key 'normal 'global (kbd "<leader>m") 'consult-bookmark)
  (evil-define-key 'normal 'global (kbd "<leader>v") 'consult-project-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>k") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>g") 'magit)
  (evil-define-key 'normal 'global (kbd "<leader>,") 'other-frame)

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
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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

;; ;; Use a thin wrapper for undo. Why?
;; ;; .. By default undo doesn't support redo as most users would expect from other software.
(require 'undo-fu)

;; ---------
;; Git
;; ---------
(use-package magit
  :config
  (setq magit-bind-magit-project-status :false)
  :init
  (with-eval-after-load 'project
    (define-key project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))
  :hook (magit-mode . (lambda ()
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

;; ---------
;; Terminal
;; ---------

(use-package vterm
  :config
  (setq vterm-shell "/usr/bin/nu")
  (add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode 0))))

(provide 'core-config)
;;; core-config.el --- ends here
