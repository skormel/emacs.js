;;; package --- Summary
;;; Commentary:
;;; Code:

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(global-set-key (kbd "C-x a i") 'indent-region)  ; automatically indent when press
(global-set-key (kbd "C-x a c") 'comment-region)  ; automatically indent when press RET
(global-set-key (kbd "C-x a u c") 'uncomment-region)  ; automatically indent when press RET

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; ;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; allow to switch from frames with shift + arrow key
(windmove-default-keybindings)

(defun indent-buffer ()
  "Indent full Buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-x a b") 'indent-buffer)

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows.
ARG argument is unkown"
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

(defun reformat-xml ()
  "Format xml."
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(defun nxml-pretty-format ()
  "Format xml."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region 0 (count-lines (point-min) (point-max)))))

(provide 'setup-global)

;;; setup-global.el ends here
