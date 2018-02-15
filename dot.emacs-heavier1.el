(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(package-selected-packages
   (quote
    (go-mode undo-tree counsel ivy projectile typescript-mode clj-refactor magit rjsx-mode paredit cider)))
 '(ring-bell-function (quote ignore))
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; To copy and paste more unicode symbols
;; see http://xahlee.info/comp/unicode_index.html
(define-key key-translation-map (kbd "<f9> e") (kbd "ε"))
(define-key key-translation-map (kbd "<f9> d") (kbd "δ"))
(define-key key-translation-map (kbd "<f9> D") (kbd "°"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load Paths
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/hand-downloaded")
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/proj/emacsenations/")
(add-to-list 'load-path "~/proj/benfu.el/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package
(require 'package)

;;;;;;;;;;;;;;;;;;;;
;;;; Best repo as of 11-Oct-2015
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
;;(add-to-list 'package-archives
;;'("melpa" . "http://melpa.milkbox.net/packages/") t)

;;;;;;;;;;;;;;;;;;;;
;;;; Not working 11-Oct-2015 (Slow and not getting full list)
;;(add-to-list 'package-archives
;;'("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;'("melpa" . "https://melpa.org/packages/") t)

;;(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
;;; Not as popular of a repo anymore I think
;;(add-to-list 'package-archives
;;'("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Require's that are dependent on the above package manager
(require 'paredit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
;; (setq emacs-lisp-mode-hook nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell Checking
(setq-default ispell-program-name "aspell")
(setq-default ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Projectile
(require 'projectile)
(require 'ivy)
(require 'counsel)
(require 'counsel-projectile)
(add-to-list 'projectile-mode-hook
	     (lambda ()
	       ;; Note sure if these toggles are needed or if
	       ;; projectile will automatically find them, etc.
	       (let ((toggle (if projectile-mode 1 -1)))
		 (ivy-mode toggle)
		 (counsel-mode toggle))
	       (if projectile-mode
		   (counsel-projectile-on)
		 (counsel-projectile-off))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Project Mode (my personal mode - not that good)
;;(require 'project-mode)
;;(project-load-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lisp (General)
(add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Cider and Clojure (for config see: https://github.com/clojure-emacs/cider/)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-hook 'clojure-mode-hook 'paredit-mode) ;; same as above
;;; Suppress auto-enabling of cider-mode in clojure-mode buffers, when starting CIDER
;;(setq cider-auto-mode nil)
;;; Don't log commo with server
;;(setq nrepl-log-messages nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; benfu.el
(require 'benfu)
(add-hook 'clojure-mode-hook 'benfu-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; emacsenations
;; (require 'project-mode)
;; (require 'clojure-project-mode)
;; (add-hook 'clojure-mode-hook 'clojure-project-mode)
;; (project-load-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ESS - Statistics (Install R first)
;; (require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'narrow-to-region 'disabled nil)
