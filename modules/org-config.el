;;; core-config.el -*- lexical-binding: t; -*-
(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/documents/roam"))
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-autosync-mode t))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(provide 'org-config)
;;; core-config.el --- ends here
