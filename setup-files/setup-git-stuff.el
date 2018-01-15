;;; package --- Summary
;;; Commentary:
;;; Code:

;; Time-stamp: <2017-12-02 14:39:56 csraghunandan>

;; https://magit.vc , https://github.com/magit/magit
;; magit: the git porcelain to manage git
(use-package magit
  :bind (("C-c m b" . magit-blame)
         ("C-c m s" . hydra-magit/body))

  :config
  (setq magit-completing-read-function 'ivy-completing-read)

  (progn
    ;; Magit Submodule support
    ;; https://www.reddit.com/r/emacs/comments/6aiwk5/how_to_manage_multiple_gitrepositories_at_once/dhf47dg/
    (dolist (fn '(;; Below will end up being the last of these newly added fns,
                  ;; and the last element in `magit-status-sections-hook' too.
                  magit-insert-modules-unpulled-from-upstream
                  magit-insert-modules-unpushed-to-pushremote
                  magit-insert-modules-unpushed-to-upstream
                  magit-insert-modules-unpulled-from-pushremote
                  ;; Below will end up being the first of these newly added fns.
                  magit-insert-submodules))
      (magit-add-section-hook 'magit-status-sections-hook `,fn nil :append)))

  (defun wh/switch-magit-status-buffer ()
    "Allow switching between open magit status buffers."
    (interactive)
    (let* ((buffers (--filter (eq #'magit-status-mode (with-current-buffer it major-mode))
                              (buffer-list)))
           (bufs-with-names (--map (cons
                                    (with-current-buffer it
                                      (projectile-project-name))
                                    it)
                                   buffers))
           (chosen-buf
            (cdr (assoc (completing-read "Git project: " bufs-with-names)
                        bufs-with-names))))
      (switch-to-buffer chosen-buf)))
  (bind-key "C-C m p" #'wh/switch-magit-status-buffer)

  (defhydra hydra-magit (:color blue
                                :columns 5)
    "Magit"
    ("g" magit-status "status")
    ("s" magit-status "status")
    ("l" magit-log-all-branches "log")
    ("b" magit-branch-popup "branch popup")
    ("r" magit-rebase-popup "rebase popup")
    ("R" magit-show-refs-popup "show refs")
    ("f" magit-fetch-popup "fetch popup")
    ("P" magit-push-popup "push popup")
    ("F" magit-pull-popup "pull popup")
    ("d" magit-diff-popup "diff popup")
    ("D" magit-diff-buffer-file-popup "diff file popup")
    ("p" magit-log-buffer-file-popup "file log popup")
    ("W" magit-format-patch "format patch")
    ("$" magit-process-buffer "process")
    ("q" nil "cancel" :color blue)))

;; git-timemachine: to rollback to different commits of files
;; https://github.com/pidu/git-timemachine
(use-package git-timemachine :defer t
  :diminish git-timemachine-mode "𝐓𝐦"
  :bind (("C-c g t" . git-timemachine-toggle)))

;; diff-hl: highlight diffs in the fringe
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (global-diff-hl-mode)
  ;; integate diff-hl with magit
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  (bind-key "C-c h d"
            (defhydra diff-hl-hunk-hydra (:color red)
              ("p" diff-hl-previous-hunk "prev hunk")
              ("n" diff-hl-next-hunk "next hunk")
              ("d" diff-hl-diff-goto-hunk "goto hunk")
              ("r" diff-hl-revert-hunk "revert hunk")
              ("m" diff-hl-mark-hunk "mark hunk")
              ("q" nil "quit" :color blue))))

;; git-messenger: popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger :defer t
  :config
  ;; Enable magit-show-commit instead of pop-to-buffer
  (setq git-messenger:use-magit-popup t)

  (bind-key "C-c g m" 'git-messenger:popup-message)
  (bind-key "m" 'git-messenger:copy-message git-messenger-map))

;; git-link: emacs package for getting the github/gitlab/bitbucket URL
;; https://github.com/sshaw/git-link
(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :bind
  ("C-c g l" . git-link)
  ("C-c g c" . git-link-commit)
  ("C-c g h" . git-link-homepage))

(use-package magit-log :ensure nil
  :init
  (progn
    ;; Set `magit-log-margin' value in :init as many other variables will be
    ;; dynamically set based on its value when `magit-log' is loaded.
    ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
    ;; Show the commit ages with 1-char time units
    ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
    ;; Also reduce the author column width to 10 as the author name is being
    ;; abbreviated below.
    (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11)))
  :config
  (progn
    ;; Abbreviate author name, show "F Last" instead of "First Last".
    ;; If author's name is just "First", don't abbreviate it.
    (defun modi/magit-log--abbreviate-author (&rest args)
      "The first arg is AUTHOR, abbreviate it.
First Last -> F Last
First      -> First (no change)."
      ;; ARGS             -> '((AUTHOR DATE))
      ;; (car ARGS)       -> '(AUTHOR DATE)
      ;; (car (car ARGS)) -> AUTHOR
      (let* ((author (car (car args)))
             (author-abbr (replace-regexp-in-string "\\(.\\).*? +\\(.*\\)" "\\1 \\2" author)))
        (setf (car (car args)) author-abbr))
      (car args))                       ;'(AUTHOR-ABBR DATE)
    (advice-add 'magit-log-format-margin :filter-args #'modi/magit-log--abbreviate-author)))

;; git-modes: major modes for git config, ignore and attributes files
;; https://github.com/magit/git-modes
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package gitattributes-mode)

(provide 'setup-git-stuff)

;; diff-hl
;; C-x v [ -> diff-hl-previous-hunk
;; C-x v ] -> diff-hl-next-hunk
;; C-x v = -> diff-hl-goto-hunk
;; C-x v n -> diff-hl-revert-hunk

;; magit
;; |---------+----------------------------------|
;; | Binding | Description                      |
;; |---------+----------------------------------|
;; | j n     | Jump to Untracked section        |
;; | j u     | Jump to Unstaged section         |
;; | j s     | Jump to Staged section           |
;; | j p     | Jump to Unpushed section         |
;; | M-p     | Jump to previous sibling section |
;; | M-n     | Jump to next sibling section     |
;; |---------+----------------------------------|

;; Tip: Adding prefix to above jump commands also expands those sections and
;; brings that section to the top of the buffer.
;;   So `C-u j s' is analogous to doing `j s C-l C-l 4`
;;; setup-git-stuff.el ends here























;; ;; (use-package magit-svn
;; ;;   :defer nil
;; ;;   :config
;; ;;   (load-theme 'arjen t t)
;; ;;   (enable-theme 'arjen)
;; ;;   )


;; ;; (defconst demo-packages
;; ;;   '(
;; ;;     2048-game
;; ;;     anzu
;; ;;     auto-complete
;; ;;     browse-kill-ring
;; ;;     clean-aindent-mode
;; ;;     color-theme-modern
;; ;;     comment-dwim-2
;; ;;     company
;; ;;     dtrt-indent
;; ;;     duplicate-thing
;; ;;     editorconfig
;; ;;     emmet-mode
;; ;;     exec-path-from-shell
;; ;;     flycheck
;; ;;     ggtags
;; ;;     haskell-mode
;; ;;     helm
;; ;;     helm-core
;; ;;     helm-css-scss
;; ;;     iedit
;; ;;     js2-mode
;; ;;     js2-refactor
;; ;;     json-mode
;; ;;     json-snatcher
;; ;;     magit
;; ;;     magit-svn
;; ;;     markdown-mode
;; ;;     multiple-cursors
;; ;;     php-auto-yasnippets
;; ;;     php-mode
;; ;;     projectile
;; ;;     pug-mode
;; ;;     restclient
;; ;;     restclient-helm
;; ;;     s
;; ;;     sass-mode
;; ;;     scss-mode
;; ;;     seq
;; ;;     smartparens
;; ;;     tern
;; ;;     tern-auto-complete
;; ;;     tide
;; ;;     typescript-mode
;; ;;     undo-tree
;; ;;     volatile-highlights
;; ;;     with-editor
;; ;;     ws-butler
;; ;;     yasnippet
;; ;;     zygospore
;; ;;     ))

;; ;; (defun install-packages ()
;; ;;   "Install all required packages."
;; ;;   (interactive)
;; ;;   (unless package-archive-contents
;; ;;     (package-refresh-contents))
;; ;;   (dolist (package demo-packages)
;; ;;     (unless (package-installed-p package)
;; ;;       (package-install package))))

;; ;; (install-packages)

;; ;; (windmove-default-keybindings)

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
;; (eval-after-load 'php-mode
;;   '(require 'php-ext))


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

;; ;; Enable magit-svn extension
;; (add-hook 'magit-mode-hook 'magit-svn-mode)

;; (provide 'setup-git-stuff)

;;; setup-git-stuff.el ends here
