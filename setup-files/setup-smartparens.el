;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package smartparens
  :defer nil
  :diminish (smartparens-mode . "𝐬")
  :config
  ;; Package: smartparens
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)

  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  )

(provide 'setup-smartparens)

;;; setup-smartparens.el ends here
