;;; package --- Summary
;;; Commentary:
;;; Code:

(defconst demo-packages
  '(
    2048-game
    ;; anzu
    auto-complete
    browse-kill-ring
    clean-aindent-mode
    ;; color-theme-modern
    comment-dwim-2
    company
    dtrt-indent
    ;; duplicate-thing
    editorconfig
    ;; emmet-mode
    exec-path-from-shell
    flycheck
    ggtags
    haskell-mode
    ;; helm
    ;; helm-core
    ;; helm-css-scss
    iedit
    js2-mode
    js2-refactor
    json-mode
    json-snatcher
    markdown-mode
    multiple-cursors
    php-auto-yasnippets
    php-mode
    projectile
    pug-mode
    restclient
    restclient-helm
    s
    sass-mode
    scss-mode
    seq
    ;; smartparens
    tern
    tern-auto-complete
    tide
    typescript-mode
    undo-tree
    volatile-highlights
    with-editor
    ws-butler
    ;; yasnippet
    ;; zygospore
    ))

(windmove-default-keybindings)

(require 'php-mode)
(eval-after-load 'php-mode
  '(require 'php-ext))


;; (global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
;; (global-set-key (kbd "C-c w") 'whitespace-mode)

;; ;; show unncessary whitespace that can mess up your diff
;; (add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; ;; use space to indent by default
;; (setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; ;; Package: yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; Package: projectile
(require 'projectile)
(projectile-mode)
(setq projectile-enable-caching t)

;; ;; Package zygospore
;; (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-eslint)))

(require 'restclient)

(require 'auto-complete-config)
(ac-config-default)
(require 'php-auto-yasnippets)
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
;;(payas/ac-setup)

(defun setup-tide-mode ()
  "Setup for tide-mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook '(lambda ()
                            (setq js2-basic-offset 2)
                            (tern-mode t)))

;; Enable autocomplete on tern-mode
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))


(require 'editorconfig)
(editorconfig-mode 1)

;; (global-anzu-mode +1)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Enable browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; (require 'emmet-mode)
;; (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; (global-set-key (kbd "C-c C-d") 'emmet-expand-line)


(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; (global-set-key (kbd "C-c C-d") 'duplicate-thing)


(require 's)


(require 'sgml-mode)

(provide 'setup-old-config)

;;; setup-old-config.el ends here
