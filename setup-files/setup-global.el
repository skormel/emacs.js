;;; package --- Summary
;;; Commentary:
;;; Code:

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(global-set-key (kbd "C-x a i") 'indent-region)  ; automatically indent when press
(global-set-key (kbd "C-x a c") 'comment-region)  ; automatically indent when press RET
(global-set-key (kbd "C-x a u c") 'uncomment-region)  ; automatically indent when press RET

(defun indent-buffer ()
  "Indent full Buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-x a b") 'indent-buffer)


(provide 'setup-global)

;;; setup-global.el ends here
