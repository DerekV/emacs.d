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


;; https://gist.github.com/dustinlacewell-wk/fc998c56c61aceda12d8e095eab83759

(require 'package)
(setq package-list '(use-package req-package))

(setq package-archives '(("MELPA" . "http://melpa.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu"  . "http://elpa.gnu.org/packages/"))
      load-prefer-newer t
      package--init-file-ensured t
      package-enable-at-startup nil)

(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t))

(package-initialize)

(setq byte-compile-warnings nil
      gnutls-min-prime-bits 4096)

(unless package-archive-contents
  (package-refresh-contents))

(setq package-list '(el-get use-package req-package))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(setq use-package-always-ensure t)

;;; end tip from gist



;; now that core packages are installed via package.el (thanks to gist)
;; we need to define some req-package clauses and then finish everything up
;; with (req-package-finish)

(req-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")


(req-package json-mode
  :ensure t
  :mode "\\.json\\'")


(req-package-finish)


;;; this just magically appears, I guess


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode json-mode req-package use-package el-get))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
