;; disable the ugly menu bar
(tool-bar-mode -1)

;; load a nice dark theme as default
(load-theme 'misterioso)

;; auto-save on loss of focus
(defun save-all ()
  (interactive)
  (save-some-buffers t))
  
(add-hook 'focus-out-hook 'save-all)


;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
