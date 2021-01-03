;;; init.el -*- lexical-binding: t; -*-
;; Time-stamp: <2021-01-03 01:51:24 adelgado>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>


;; Set this varibles to make elpa works with PROXY
;; (setq url-proxy-services
;;       '(("no-proxy" . "^\\(localhost|10.*\\)")
;;     ("http" . "dns:port")
;;     ("https" . "dns:port")))

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar rag--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))

;; set home and emacs directories
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

(load (locate-user-emacs-file "general.el") nil :nomessage)

;; load all use-package related configuration
(load (locate-user-emacs-file "setup-packages.el") nil :nomessage)

;; Install al setup files here
(require 'setup-optimizations)
(require 'setup-no-littering)
(require 'setup-hydra)
(require 'setup-editor)
(require 'setup-global)
(require 'setup-theme)
(require 'setup-org)
;; (require 'setup-beacon)
(require 'setup-highlight)
(require 'setup-zygospore)
(require 'setup-duplicate-thing)
(require 'setup-smartparens)
(require 'setup-anzu)
(require 'setup-helm)
(require 'setup-emmet)
(require 'setup-git)
(require 'setup-lsp)
(require 'setup-haskell)
(require 'setup-editorconfig)
(require 'setup-games)
(require 'setup-js)
(require 'setup-json)
(require 'setup-typescript)
(require 'setup-docker)
(require 'setup-kubernetes)
;; (require 'setup-company)
(require 'setup-osx)
;; (require 'setup-backup)
;; (require 'setup-selected)
;; (require 'setup-treemacs)
;; (require 'setup-search)
;; (require 'setup-rg)
;; (require 'setup-ibuffer)
;; (require 'setup-recentf)
;; (require 'setup-desktop)
;; (require 'setup-calc)
;; (require 'setup-ediff)
;; (require 'setup-dired)
;; (require 'setup-elisp-mode)
;; (require 'setup-flycheck)
;; (require 'setup-spell)
;; (require 'setup-bookmark)
;; (require 'setup-avy)
(require 'setup-window)
;; (require 'setup-project)
;; (require 'setup-yas)
(require 'setup-buffers)
;; (require 'setup-ivy)
;; (require 'setup-counsel)
;; (require 'setup-movement)
;; (require 'setup-markdown)
;; (require 'setup-plantuml)
;; (require 'setup-info)
;; (require 'setup-mode-line)
(require 'setup-editing)
;; (require 'setup-racket)
;; (require 'setup-rust)
;; (require 'setup-go)
;; (require 'setup-cc)
;; (require 'setup-python)
;; (require 'setup-tex)
;; (require 'setup-origami)
;; (require 'setup-white-space)
;; (require 'setup-mc)
;; (require 'setup-ocaml)
;; (require 'setup-web-mode)
;; (require 'setup-css)
(require 'setup-eshell)
;; (require 'setup-comint)
;; (require 'setup-vterm)
(require 'setup-which-key)
;; (require 'setup-kurecolor)
;; (require 'setup-font-check)
(require 'setup-misc)
(require 'setup-visual)
;; (require 'setup-tree-sitter)
(require 'setup-tramp)
;; (require 'setup-calendar)
;; (require 'setup-minibuffer)
;; (require 'setup-purescript)
;; (require 'setup-lua)
;; (require 'setup-abbrev)
;; (require 'setup-compile)
;; (require 'setup-macro)
;; (require 'setup-help)
;; (require 'setup-ansible)
(require 'setup-sh)
;; (require 'setup-comb)
;; (require 'setup-smerge)
;; (require 'setup-nov)
;; (require 'setup-xkcd)
;; (require 'setup-pdf)
;; (require 'setup-engine-mode)
;; (require 'setup-config-files)

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



;; OLD CONFIG

;; (setq gc-cons-threshold 100000000)

;; (defconst demo-packages
;;   '(
;;     auto-complete
;;     clean-aindent-mode
;;     color-theme-modern
;;     comment-dwim-2
;;     company
;;     docker
;;     dockerfile-mode
;;     dtrt-indent
;;     exec-path-from-shell
;;     flycheck
;;     ggtags
;;     haskell-mode
;;     iedit
;;     js2-mode
;;     js2-refactor
;;     json-mode
;;     json-snatcher
;;     markdown-mode
;;     multiple-cursors
;;     prettier-js
;;     projectile
;;     pug-mode
;;     restclient
;;     s
;;     seq
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
;;     ))


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

;; ;; Package: projectile
;; (require 'projectile)
;; (projectile-mode)
;; (setq projectile-enable-caching t)

;; ;; http://www.flycheck.org/manual/latest/index.html
;; (require 'flycheck)

;; ;; turn on flychecking globally
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; ;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-eslint)))

;; (require 'auto-complete-config)
;; (ac-config-default)
;; ;;(payas/ac-setup)

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

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js2-mode-hook '(lambda ()
;;                             (setq js2-basic-offset 2)
;;                             (tern-mode t)))

;; ;; Enable autocomplete on tern-mode
;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))


;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; (require 's)

;; (require 'sgml-mode)

;; (require 'prettier-js)

;; (add-hook 'tide-mode-hook 'prettier-js-mode)
;; (add-hook 'js2-mode-hook 'prettier-js-mode)

;; (setq prettier-js-args '(
;;   "--single-quote" "true"
;;   "--print-width" "140"
;; ))

;; ;;; init.el ends here
