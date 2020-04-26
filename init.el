;;; init.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-04-07 18:34:33 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>


;; Set this varibles to make elpa works with PROXY
;; (setq url-proxy-services
;;       '(("no-proxy" . "^\\(localhost|10.*\\)")
;;     ("http" . "dns:port")
;;     ("https" . "dns:port")))

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar rag--file-name-handler-alist file-name-handler-alist)

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun rag-set-gc-threshold ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold gc-cons-threshold--orig
        gc-cons-percentage 0.1
        file-name-handler-alist rag--file-name-handler-alist))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))
(add-to-list 'load-path (concat user-emacs-directory "my-elisp-code"))

;; set home and emacs directories
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

;; save custom file to a separate directory
(setq custom-file (concat user-emacs-directory "my-elisp-code/custom-settings.el"))
(load custom-file :noerror :nomessage) ; load custom-file silently
(load (locate-user-emacs-file "general.el") nil :nomessage)

;; run package-initialize if running emacs version < 27
(>=e "27.0"
    nil
  (package-initialize))


;; load all use-package related configuration
(load (locate-user-emacs-file "setup-packages.el") nil :nomessage)


;; Install al setup files here
(require 'setup-optimizations)
(require 'setup-no-littering)
(require 'setup-org)


;; END setup files


;; install all packages (if they already not installed by use-package)
(package-install-selected-packages)

;; start emacs server only it has not already been started
(require 'server)
(unless (server-running-p) (server-start))

;; set gc-cons-threshold back to original value
(setq file-name-handler-alist rag--file-name-handler-alist
      gc-cons-threshold 16777216 ;; use 16MB
      gc-cons-percentage 0.1)
;; enable gchm mode
(gcmh-mode +1)
;; garbage collect when moving out to other applications
(add-function :after after-focus-change-function #'gcmh-idle-garbage-collect)

;;; init.el ends here

;; ;;; package --- Summary
;; ;;; Commentary:
;; ;;; Code:


;; ;; Set this varibles to make elpa works with PROXY
;; ;; (setq url-proxy-services
;; ;;       '(("no-proxy" . "^\\(localhost|10.*\\)")
;; ;;     ("http" . "dns:port")
;; ;;     ("https" . "dns:port")))

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)

;; (setq gc-cons-threshold 100000000)
;; (setq inhibit-startup-message t)

;; (defalias 'yes-or-no-p 'y-or-n-p)

;; (require 'linum)
;; (setq linum-format "%d ")
;; (add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; (menu-bar-mode 0)
;; (scroll-bar-mode 0)
;; (tool-bar-mode 0)
;; (setq column-number-mode t)

;; (defconst demo-packages
;;   '(
;;     2048-game
;;     anzu
;;     auto-complete
;;     beacon
;;     browse-kill-ring
;;     clean-aindent-mode
;;     color-theme-modern
;;     comment-dwim-2
;;     company
;;     docker
;;     dockerfile-mode
;;     dtrt-indent
;;     duplicate-thing
;;     editorconfig
;;     emmet-mode
;;     exec-path-from-shell
;;     flycheck
;;     ggtags
;;     haskell-mode
;;     helm
;;     helm-core
;;     helm-css-scss
;;     iedit
;;     js2-mode
;;     js2-refactor
;;     json-mode
;;     json-snatcher
;;     magit
;;     magit-svn
;;     markdown-mode
;;     multiple-cursors
;;     php-auto-yasnippets
;;     php-mode
;;     prettier-js
;;     projectile
;;     pug-mode
;;     restclient
;;     restclient-helm
;;     s
;;     sass-mode
;;     scss-mode
;;     seq
;;     smartparens
;;     tern
;;     tern-auto-complete
;;     tide
;;     typescript-mode
;;     undo-tree
;;     volatile-highlights
;;     web-mode
;;     with-editor
;;     ws-butler
;;     yasnippet
;;     zygospore
;;     ))

;; (defun install-packages ()
;;   "Install all required packages."
;;   (interactive)
;;   (unless package-archive-contents
;;     (package-refresh-contents))
;;   (dolist (package demo-packages)
;;     (unless (package-installed-p package)
;;       (package-install package))))

;; (install-packages)

;; (windmove-default-keybindings)

;; ;; Load custom scripts from custom directory
;; (add-to-list 'load-path "~/.emacs.d/custom")

;; (require 'helm-config)
;; (helm-mode 1)

;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-z") 'helm-select-action)

;; (require 'php-mode)
;; ;; (eval-after-load 'php-mode
;; ;;   '(require 'php-ext))


;; (global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; ;; activate whitespace-mode to view all whitespace characters
;; (global-set-key (kbd "C-c w") 'whitespace-mode)

;; ;; show unncessary whitespace that can mess up your diff
;; (add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; ;; use space to indent by default
;; (setq-default indent-tabs-mode nil)

;; ;; set appearance of a tab that is represented by 4 spaces
;; (setq-default tab-width 4)

;; ;; Package: clean-aindent-mode
;; (require 'clean-aindent-mode)
;; (add-hook 'prog-mode-hook 'clean-aindent-mode)

;; ;; Package: dtrt-indent
;; (require 'dtrt-indent)
;; (dtrt-indent-mode 1)

;; ;; Package: ws-butler
;; (require 'ws-butler)
;; (add-hook 'prog-mode-hook 'ws-butler-mode)

;; ;; Package: yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;; Package: smartparens
;; (require 'smartparens-config)
;; (setq sp-base-key-bindings 'paredit)
;; (setq sp-autoskip-closing-pair 'always)
;; (setq sp-hybrid-kill-entire-symbol nil)
;; (sp-use-paredit-bindings)

;; (show-smartparens-global-mode +1)
;; (smartparens-global-mode 1)

;; ;; Package: projectile
;; (require 'projectile)
;; (projectile-mode)
;; (setq projectile-enable-caching t)

;; ;; Package zygospore
;; (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
;; (global-set-key (kbd "C-x g") 'magit-status)
;; (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; (global-set-key (kbd "C-x a i") 'indent-region)  ; automatically indent when press
;; (global-set-key (kbd "C-x a c") 'comment-region)  ; automatically indent when press RET
;; (global-set-key (kbd "C-x a u c") 'uncomment-region)  ; automatically indent when press RET

;; (load-theme 'arjen t t)
;; (enable-theme 'arjen)

;; ;; http://www.flycheck.org/manual/latest/index.html
;; (require 'flycheck)

;; ;; turn on flychecking globally
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; ;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-eslint)))

;; (require 'restclient)

;; (require 'auto-complete-config)
;; (ac-config-default)
;; (require 'php-auto-yasnippets)
;; (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
;; ;;(payas/ac-setup)

;; (defun transpose-windows (arg)
;;   "Transpose the buffers shown in two windows.
;; ARG argument is unkown"
;;   (interactive "p")
;;   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
;;     (while (/= arg 0)
;;       (let ((this-win (window-buffer))
;;             (next-win (window-buffer (funcall selector))))
;;         (set-window-buffer (selected-window) next-win)
;;         (set-window-buffer (funcall selector) this-win)
;;         (select-window (funcall selector)))
;;       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; (define-key ctl-x-4-map (kbd "t") 'transpose-windows)

;; (defun kill-other-buffers ()
;;   "Kill all other buffers."
;;   (interactive)
;;   (mapc 'kill-buffer
;;         (delq (current-buffer)
;;               (remove-if-not 'buffer-file-name (buffer-list)))))

;; (defun setup-tide-mode ()
;;   "Setup for tide-mode."
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   ;; (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
;;   (eldoc-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ;; format options
;; (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))


;; (defun indent-buffer ()
;;   "Indent full Buffer."
;;   (interactive)
;;   (delete-trailing-whitespace)
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max)))

;; (global-set-key (kbd "C-x a b") 'indent-buffer)

;; (require 'helm-css-scss)
;; ;; Allow comment inserting depth at each of a brace
;; (setq helm-css-scss-insert-close-comment-depth 2)
;; ;; If this value is t, split window appers inside the current windows
;; (setq helm-css-scss-split-with-multiple-windows nil)
;; ;; Split direction. 'split-window-vertically or 'split-window-horizontally
;; (setq helm-css-scss-insert-split-direction 'split-window-vertically)

;; ;; Set local keybind map for css-mode / scss-mode / less-css-mode
;; (dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
;;   (add-hook
;;    $hook (lambda ()
;;            (local-set-key (kbd "s-i") 'helm-css-scss)
;;            (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

;; (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
;; (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js2-mode-hook '(lambda ()
;;                             (setq js2-basic-offset 2)
;;                             (tern-mode t)))

;; ;; Enable autocomplete on tern-mode
;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))


;; (require 'editorconfig)
;; (editorconfig-mode 1)

;; (global-anzu-mode +1)

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

;; ;; Enable browse-kill-ring
;; (require 'browse-kill-ring)
;; (browse-kill-ring-default-keybindings)

;; (require 'emmet-mode)
;; (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; (global-set-key (kbd "C-c C-d") 'emmet-expand-line)


;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; (global-set-key (kbd "C-c C-d") 'duplicate-thing)


;; (require 's)


;; (require 'sgml-mode)

;; (defun reformat-xml ()
;;   "Format xml."
;;   (interactive)
;;   (save-excursion
;;     (sgml-pretty-print (point-min) (point-max))
;;     (indent-region (point-min) (point-max))))

;; (defun nxml-pretty-format ()
;;   "Format xml."
;;   (interactive)
;;   (save-excursion
;;     (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
;;     (nxml-mode)
;;     (indent-region 0 (count-lines (point-min) (point-max)))))

;; (defun indent-marked-files ()
;;   "Autoindent all selected files."
;;   (interactive)
;;   (dolist (file (dired-get-marked-files))
;;     (find-file file)
;;     (indent-region (point-min) (point-max))
;;     (save-buffer)
;;     (kill-buffer nil)))

;; ;; Enable magit-svn extension
;; (add-hook 'magit-mode-hook 'magit-svn-mode)

;; (beacon-mode)
;; (setq beacon-size 25)
;; (add-to-list 'beacon-dont-blink-major-modes 'comint-mode)

;; (require 'prettier-js)

;; (add-hook 'tide-mode-hook 'prettier-js-mode)
;; (add-hook 'js2-mode-hook 'prettier-js-mode)

;; ;; (setq prettier-js-args '(
;; ;;   "--single-quote" "true"
;; ;;   "--print-width" "140"
;; ;; ))

;; ;;; init.el ends here
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(docker-compose-mode pug-mode zygospore ws-butler web-mode volatile-highlights undo-tree tide tern-auto-complete smartparens scss-mode sass-mode restclient-helm projectile prettier-js php-auto-yasnippets markdown-mode magit-svn json-mode js2-refactor iedit helm-css-scss haskell-mode ggtags exec-path-from-shell emmet-mode editorconfig duplicate-thing dtrt-indent company comment-dwim-2 color-theme-modern clean-aindent-mode browse-kill-ring beacon anzu 2048-game)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
