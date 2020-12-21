(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
;; (use-package kubernetes-evil
;;   :ensure t
;;   :after kubernetes)

(use-package kubernetes-tramp
  :defer t)

(provide 'setup-kubernetes)
