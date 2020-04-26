;;; package --- Summary
;;; Commentary:
;;; Code:

;; (setq inhibit-startup-message t)

;; (defalias 'yes-or-no-p 'y-or-n-p)

;; (require 'linum)
;; (setq linum-format "%d ")
;; (add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; (menu-bar-mode 0)
;; (scroll-bar-mode 0)
;; (tool-bar-mode 0)
;; (setq column-number-mode t)

(use-package color-theme-modern
  :defer nil
  :config
  (load-theme 'arjen t t)
  (enable-theme 'arjen)
  )

(provide 'setup-theme)

;;; setup-theme.el ends here
