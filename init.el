;;; package --- Summary
;;; Commentary:
;;; Code:
;; (setq url-proxy-services
;;       '(("no-proxy" . "^\\(localhost|10.*\\)")
;;     ("http" . "dns:port")
;;     ("https" . "dns:port")))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'linum)
(setq linum-format "%d ")
;(autoload 'linum "linum" "Line numbers for buffers." t)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(setq column-number-mode t)

(defconst demo-packages
  '(anzu
    company
    duplicate-thing
    ggtags
    ;; function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    2048-game
    flycheck
    magit
    color-theme-modern
    markdown-mode
    js2-mode
    json-mode
    tide
    helm
    helm-core
    helm-css-scss
    sass-mode
    scss-mode
    editorconfig
    zygospore))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(windmove-default-keybindings)

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/custom/skeleton")
(add-to-list 'load-path "~/.emacs.d/custom/ac-dict")
(add-to-list 'load-path "~/.emacs.d/custom/helm")

(require 'helm-config)
(helm-mode 1)

(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)



(require 'php-mode)
(eval-after-load 'php-mode
  '(require 'php-ext))


(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(global-set-key (kbd "C-x a i") 'indent-region)  ; automatically indent when press
(global-set-key (kbd "C-x a c") 'comment-region)  ; automatically indent when press RET
(global-set-key (kbd "C-x a u c") 'uncomment-region)  ; automatically indent when press RET

(load-theme 'arjen t t)
(enable-theme 'arjen)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-eslint)))

(eval-after-load 'php-mode
  '(require 'php-ext))

(require 'restclient)

(require 'auto-complete-config)
(ac-config-default)
(require 'php-auto-yasnippets)
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
;;(payas/ac-setup)

 (defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next lines, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1))) ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position))))
      (if (> 0 n) ; comment out original with negative arg
          (comment-region (line-beginning-position) (line-end-position)))
      (forward-line 1)
      (forward-char pos)))))

(global-set-key (kbd "C-c C-d") 'duplicate-line-or-region)


(defun setup-tide-mode ()
  "Setup for tide-mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
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


;;; init.el ends here

(defun indent-buffer ()
  "Indent full Buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-x a b") 'indent-buffer)

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

(global-anzu-mode +1)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Enable browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zygospore yasnippet ws-butler volatile-highlights undo-tree tide tern-auto-complete smartparens scss-mode sass-mode projectile markdown-mode magit json-mode js2-mode iedit helm-css-scss ggtags exec-path-from-shell editorconfig duplicate-thing dtrt-indent company comment-dwim-2 color-theme-modern clean-aindent-mode browse-kill-ring anzu 2048-game))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
