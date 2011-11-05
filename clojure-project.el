;;; clojure-project.el --- Extends project-mode for Clojure projects
;;
;; Copyright 2011-2012 Benjamin Cluff
;;
;; Author: Benjamin Cluff <psyllo@gmail.com>
;; URL: https://github.com/psyllo/emacsenations
;; Created: 03-Nov-2011
;; Version: 1.0
;; Package-Requires: ((project-mode "1.0"))
;;
;; Synopsis: Extends project-mode for Clojure specific needs.
;;
;; Installation:
;; (require 'clojure-project)
;; (setq project-mode t)
;; (add-hook 'emacs-startup-hook
;;           (lambda nil
;;             (run-mode-hooks 'project-mode-hook)
;;             (project-load-all) ; Loads all saved projects. Not required.
;;             (clojure-project-init)))
;;

(require 'project-mode)

(defcustom project-tags-form-clojure
  '(".clj$"
    ('elisp ("(def[a-z0-9$?<>+*!_-]*[ \r\n\t]+\\([a-z0-9$?<>+*!_-]+\\)" 0)))
  "Overwrites `PROJECT-TAGS-FORM-DEFAULT' for generation of TAGS."
  :group 'project-mode)

(defun clojure-project-core-name-for-file (file-name)
  (project-file-basename
   (substring file-name 0
              (string-match "\\([_-]test\\.clj\\|\\.clj\\)"
                            file-name))))

(defun clojure-project-find-test-file-for (file-arg)
  (let ((core-name (clojure-project-core-name-for-file file-arg)))
    (dolist (file (project-path-cache-get (project-current)))
      (when (and (string-equal core-name (clojure-project-core-name-for-file file))
                 (project-dir-in-file-path-p file "test"))
        (message (concat "Found test file '" (project-file-basename file)
                         "' for '" (project-file-basename file-arg) "'"))
        (return file)))))

(defun clojure-project-find-unit-test-for-current nil
  (interactive)
  (project-ensure-current)
  (let ((file (clojure-project-find-test-file-for (buffer-file-name))))
    (if file
        (find-file file)
      (message (concat "Could not find unit test for the current buffer.")))))

(defun clojure-project-init nil
  (interactive)
  (when project-mode
    (let ((k-def '("Find Unit Test For Current" . clojure-project-find-unit-test-for-current)))
      (project-add-to-tags-form (project-current)
                                (first project-tags-form-clojure)
                                (second project-tags-form-clojure))
      (define-key global-map [C-f10] k-def)
      (define-key
        global-map
        [menu-bar projmenu projsrch cljprjunit9z] k-def))))

(provide 'clojure-project)

;;; clojure-project.el ends here
