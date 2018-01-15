;; disable the ugly menu bar
(tool-bar-mode -1)

;; avoid using tabs for indentation, by default
;; https://www.emacswiki.org/emacs/TabsAreEvil
;; https://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil)


;; load a nice dark theme as default
(load-theme 'misterioso)

;; show matching braces/parans
(setq show-paren-delay 0)
(show-paren-mode 1)


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

;; http://oremacs.com/swiper/  
(req-package ivy
  :ensure t
  :config
  ;; turns ivy on eveywhere
  (ivy-mode 1)
  ;; these next two are "suggested minimal customizations"
  ;; virtual buffers remembers files you opened in the past and shows them when you switch buffers
  (setq ivy-use-virtual-buffers t)
  ;; I assume this just tweaks the output slightly
  (setq ivy-count-format "(%d/%d) "))

(req-package swiper
  :ensure t
  :require ivy)

(req-package counsel
  :ensure t
  :require ivy swiper)

;; https://github.com/technomancy/find-file-in-project
(req-package find-file-in-project
  :ensure t
  :require ivy
  :config
  (global-set-key (kbd "C-c p o") 'find-file-in-project))
  

;; https://github.com/purcell/exec-path-from-shell
(req-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; https://github.com/mhayashi1120/Emacs-wgrep
(req-package wgrep
  :ensure t)

;; company "complete anything" http://company-mode.github.io/
(req-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode) ;; enable everwhere
  )

(req-package magit
  :ensure t)

 
(req-package-finish)



;; ---  more customizations ---

;; its nice to know the full title of the file you are working on.

;; https://stackoverflow.com/a/3669681/710040
;; show full file name in frame title bar
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; additionally, define a function which displays the current buffer's full path
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "C-c i p") 'show-file-name)



;; set up a way to quickly search the whole project,
;;  combining find-file-in-project and counsel-ag
(defun counsel-ag-project ()
  "Search using counsel-ag from root of project"
  (interactive)
  (counsel-ag nil (ffip-get-project-root-directory)))
(global-set-key (kbd "C-c p s") 'counsel-ag-project)

(defun dired-project-root ()
  "Open dired in project root"
  (interactive)
  (dired (ffip-get-project-root-directory)))
(global-set-key (kbd "C-c p d") 'dired-project-root)



;; -- custom set variables stuff --
;;; this just magically appears, I guess


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit company wgrep exec-path-from-shell counsel swiper find-file-in-project ivy yaml-mode json-mode req-package use-package el-get)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
