;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package helm
  :defer nil
  :config
  (helm-mode 1)
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  )

(use-package helm-core
  :defer nil
  :config
  )

(use-package helm-css-scss
  :defer nil
  :config
  (require 'helm-css-scss)
  ;; Allow comment inserting depth at each of a brace
  (setq helm-css-scss-insert-close-comment-depth 2)
  ;; If this value is t, split window appers inside the current windows
  (setq helm-css-scss-split-with-multiple-windows nil)
  ;; Split direction. 'split-window-vertically or 'split-window-horizontally
  (setq helm-css-scss-insert-split-direction 'split-window-vertically)

  ;; Set local keybind map for css-mode / scss-mode / less-css-mode
  (dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
    (add-hook
     $hook (lambda ()
             (local-set-key (kbd "s-i") 'helm-css-scss)
             (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

  (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
  (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)
  )

(use-package helm-css-scss
  :defer nil
  :config
  )

(provide 'setup-helm)

;;; setup-helm.el ends here
