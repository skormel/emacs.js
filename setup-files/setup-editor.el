(setq inhibit-startup-message t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(setq column-number-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; ;; Enable browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings)
  )

;; (require 'browse-kill-ring)
;; (browse-kill-ring-default-keybindings)

(provide 'setup-editor)
