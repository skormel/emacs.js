;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package emmet-mode
  :defer nil
  :diminish (smartparens-mode . "em")
  :bind (("C-c C-d" . emmet-expand-line))
  :hook (sgml-mode css-mode )
  )

(provide 'setup-emmet)

;;; setup-emmet.el ends here
