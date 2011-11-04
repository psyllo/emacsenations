;;; clojure-project-mode.el
;;
;; Author: Benjamin Cluff
;; Created: 03-Nov-2011
;;
;; Synopsis: Extends project-mode for Clojure specific needs.
;;

(require 'project-mode)

(defcustom project-tags-form-clojure
  '(".clj$"
    ('elisp ("(def[a-z0-9$?<>+*!_-]*[ \r\n\t]+\\([a-z0-9$?<>+*!_-]+\\)" 1)))
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

(defun clojure-project-mode-init nil
  (interactive)
  (let ((k-def '("Find Unit Test For Current" . clojure-project-find-unit-test-for-current)))
    (setq project-tags-form-default
          project-tags-form-clojure)
    (when project-mode
      (define-key global-map [C-f10] k-def)
      (define-key
        global-map
        [menu-bar projmenu projsrch cljprjunit9z] k-def))))
