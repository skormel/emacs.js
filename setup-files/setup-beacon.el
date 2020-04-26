;; beacon :-  blink the cursor whenever scrolling or switching between windows	
;; https://github.com/Malabarba/beacon	
(use-package beacon	
  :defer 1	
  :diminish beacon-mode	
  :bind (("C-!" . beacon-blink))	
  :config	
  (beacon-mode 1)	
  (setq beacon-size 50)	
  ;; don't blink in shell-mode	
  (add-to-list 'beacon-dont-blink-major-modes 'comint-mode))	

(provide 'setup-beacon)