;; (use-package linum
;;   :config
;;   (global-linum-mode t))


(use-package linum
  :config
  (setq linum-format "%d ")
  (add-hook 'find-file-hook (lambda () (linum-mode 1)))
  )

(provide 'setup-linum)

;; (require 'linum)
;; (setq linum-format "%d ")
;; (add-hook 'find-file-hook (lambda () (linum-mode 1)))
