;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package emmet-mode
  :defer nil
  :diminish (smartparens-mode . "em")
  :config
  (require 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  :bind (("C-c C-d" . emmet-expand-line)
         )
  )

(provide 'setup-emmet)

;;; setup-emmet.el ends here
