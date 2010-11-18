;;; grails-mode.el
;;
;; Author: Benjamin Cluff
;; Created: 02-Feb-2010
;;
;; Synopsis:
;;   Help when working with grails apps.
;;   * Finding files is greatly simplified (see key bindings)
;;
;; TODO:
;; - Lots of logic queues off of buffer name, but if buffer name is different
;;   because, perhaps, the user has the full path of the file as the buffer name
;;   then lots of code might fail.

(require 'project-mode)

(defgroup grails nil
  "Grails mode helps when working with grails apps."
  :prefix "grails-"
  :group 'programming)

(define-minor-mode grails-mode
  "Toggle grails mode.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Grails"
  ;; This mode is best as a global minor mode
  :global t
  ;; The minor mode bindings.
  :keymap
  '(("\M-+gd" . grails-find-domain)
    ("\M-+gc" . grails-find-controller)
    ("\M-+gs" . grails-find-service)
    ("\M-+gf" . grails-find-file-for-stacktrace-line)
    ([C-f6] . grails-find-domain-for-current)
    ([C-f7] . grails-find-controller-for-current)
    ([C-f8] . grails-find-view-for-controller-action)
    ([C-f9] . grails-find-service-for-current)
    ([C-f10] . grails-find-unit-test-for-current)
    ([C-M-f10] . grails-find-integration-test-for-current)
    ([C-f11] . grails-run-unit-test-for-current)
    ([C-M-f11] . grails-run-integration-test-for-current)
    ([C-f12] . grails-run-last-test)
    ([C-M-f12] . grails-rerun-last-test))
  :group 'grails)

(defface grails-test-failed
  '((default (:foreground "white" :background "red4" :stipple nil)))
  "Used for when a test fails.")

(defface grails-test-passed
  '((default (:foreground "white" :background "green4" :stipple nil)))
  "Used for when a test passes.")

;;; Hooks
(add-hook 'grails-mode-hook 'grails-mode-menu)
(add-hook 'emacs-startup-hook (lambda nil (run-hooks 'grails-mode-hook)))


;;;###autoload


(setq project-tags-form-default
      `(;; File name pattern
        "\\.groovy$"
        ;; Regexes that are run for the file name match
        (;; classes
         "^class\s+\\w+"
         ;; members
         ,(concat "^\\(\s\\{" (number-to-string tab-width) "\\}\\|\t\\)\\w+\s+\\w+[^\r\n$({=]+[({=]?")
         ;; closures defined in method bodies
         ,(concat "^\\(\s\\{" (number-to-string (* 2 tab-width)) "\\}\\|\t\t\\)\\w+\\.\\w+\s*=\s*{")
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive commands

(defun grails-find-domain (file-name)
  (interactive "MSearch for Domain: ")
  (project-ensure-current)
  (grails-find-domain-for file-name))

(defun grails-find-service (file-name)
  (interactive "MSearch for Service: ")
  (project-ensure-current)
  (grails-find-service-for file-name))

(defun grails-find-controller (file-name)
  (interactive "MSearch for Controller: ")
  (project-ensure-current)
  (grails-find-controller-for file-name))

(defun grails-find-domain-for-current nil
  (interactive)
  (project-ensure-current)
  (grails-find-domain-for (buffer-file-name)))

(defun grails-find-service-for-current nil
  (interactive)
  (project-ensure-current)
  (grails-find-service-for (buffer-file-name)))

(defun grails-find-controller-for-current nil
  (interactive)
  (project-ensure-current)
  (grails-find-controller-for (buffer-file-name)))

(defun grails-find-unit-test-for-current nil
  (interactive)
  (project-ensure-current)
  (let ((file (grails-find-test-file-for (buffer-file-name) "unit")))
    (if file
        (find-file file)
      (message (concat "Could not find unit test for: " (buffer-file-name))))))

(defun grails-find-integration-test-for-current nil
  (interactive)
  (project-ensure-current)
  (let ((file (grails-find-test-file-for (buffer-file-name) "integration")))
    (if file
        (find-file file)
      (message (concat "Could not find integration test for: " (buffer-file-name))))))

(defun grails-run-unit-test-for-current nil
  (interactive)
  (project-ensure-current)
  (grails-run-test-for (grails-app-base-dir-for-file (buffer-file-name))
                       (buffer-file-name) "unit" nil))

(defun grails-run-integration-test-for-current nil
  (interactive)
  (project-ensure-current)
  (grails-run-test-for (grails-app-base-dir-for-file (buffer-file-name))
                       (buffer-file-name) "integration" nil))

(defun grails-run-last-test nil
  (interactive)
  (project-ensure-current)
  (let ((args (grails-project-get-last-test)))
    (if (> (length args) 0)
        (grails-run-test-for (second (assoc :base-dir args))
                             (second (assoc :file args))
                             (second (assoc :test-phase args))
                             nil)
      (message (concat "There was no previous test run for project `" (project-current-name) "'")))))

(defun grails-rerun-last-test nil
  (interactive)
  (project-ensure-current)
  (let ((args (grails-project-get-last-test)))
    (if (> (length args) 0)
        (grails-run-test-for (second (assoc :base-dir args))
                             (second (assoc :file args))
                             (second (assoc :test-phase args))
                             t)
      (message (concat "There was no previous test run for project `" (project-current-name) "'")))))

(defun grails-find-view-for-controller-action nil
  (interactive)
  (project-ensure-current)
  (when (string-match "Controller\\." (project-buffer-name-without-<x>))
    (grails-find-view-for-controller-action-at-point (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-interactive functions

(defun* grails-project-set-last-test (&key base-dir file test-phase)
  (project-put (project-current) 'grails-last-test (list (list :base-dir base-dir)
                                                         (list :file file)
                                                         (list :test-phase test-phase))))

(defun grails-project-get-last-test nil
  (project-get (project-current) 'grails-last-test))

(defun grails-test-name-for-file (file-name)
  (project-file-basename (substring file-name 0 (string-match "Tests?\\." file-name))))

(defun grails-core-name-for-file (file-name)
  (project-file-basename
   (substring file-name 0
              (string-match "\\(Service\\.groovy\\|Controller\\.groovy\\|UnitTests?\\.groovy\\|IntegrationTests?\\.groovy\\|Tests?\\.groovy\\|\\.groovy\\)"
                            file-name))))

(defun grails-core-name-for-test-file (file-name)
  (project-file-basename
   (substring file-name 0
              (string-match "\\(UnitTests?\\.groovy\\|IntegrationTests?\\.groovy\\|Tests?\\.groovy\\|\\.groovy\\)"
                            file-name))))

(defun grails-find-domain-for (file-arg)
  (let ((file-name (concat (grails-core-name-for-file file-arg) ".groovy")))
    (if (not (project-exact-search file-name))
        (message (concat "Domain model '" file-name "' doesn't exist."))
      (message (concat "Found domain model: " file-name)))))

(defun grails-find-service-for (file-arg)
  (let ((file-name (concat (grails-core-name-for-file file-arg) "Service.groovy")))
    (if (not (project-exact-search file-name))
        (message (concat "Service '" file-name "' doesn't exist."))
      (message (concat "Found service: " file-name)))))

(defun grails-find-controller-for (file-arg)
  (let ((file-name (concat (grails-core-name-for-file file-arg) "Controller.groovy")))
    (if (not (project-exact-search file-name))
        (message (concat "Controller '" file-name "' doesn't exist."))
      (message (concat "Found controller: " file-name)))))

(defun grails-find-test-file-for (file-arg test-phase)
  (let ((core-name (grails-core-name-for-test-file file-arg)))
    (dolist (file (project-path-cache-get (project-current)))
      (when (and (string-equal core-name (grails-core-name-for-file file))
                 (project-dir-in-file-path-p file test-phase))
        (message (concat "Found " test-phase " test file '" (project-file-basename file)
                         "' for '" (project-file-basename file-arg) "'"))
        (return file)))))

(defun grails-run-test-for (base-dir file-arg test-phase rerun-p)
  (grails-project-set-last-test :base-dir base-dir
                                :file file-arg
                                :test-phase test-phase)
  (let ((test-name (grails-test-name-for-file (grails-find-test-file-for file-arg test-phase))))
    (let ((buf (get-buffer-create (concat "*" test-phase "-test-" (project-current-name) "*"))))
      (pop-to-buffer buf)
      (kill-region (point-min) (point-max))
      (local-set-key "q" 'kill-this-buffer)
      (local-set-key "Q" 'kill-buffer-and-window)
      (cd base-dir)
      (let ((proc (start-process-shell-command (concat test-phase "-test-" (project-current-name))
                                               buf
                                               "grails" "test-app" test-name (concat "-" test-phase) (when rerun-p "-rerun"))))
        (set-process-filter proc (lambda (proc str) (grails-test-filter proc str)))
        (process-kill-without-query proc)
        (message (concat "Running " test-phase " test: '" test-name "'"))))))

(defun grails-test-filter (proc test-output)
  (save-excursion
    (let ((buf (process-buffer proc)))
      (set-buffer buf)
      (grails-test-filter-insert-test-results)
      (grails-test-filter-buttonize-groovyc-errors)
      (when (string-match "exited abnormally with code 255" test-output)
        (message (concat "Error: You're probably running 'grails test-app' from the wrong directory")))
      (goto-char (point-max))
      (insert (concat "\n" test-output)))))

(defun grails-test-filter-buttonize-groovyc-errors nil
  "Meant to be called from `GRAILS-TEST-FILTER'"
  (when (string-match "\\[groovyc\\][^\r\n]+startup failed, +\\([^\r\n]+\\): +\\([0-9]+\\):" test-output)
    (beginning-of-buffer)
    (insert-button (concat (match-string-no-properties 1 test-output)
                           ":"
                           (match-string-no-properties 2 test-output))
                   'action 'project-file-line-button-handler)))

(defun grails-test-filter-insert-test-results nil
  "Meant to be called from `GRAILS-TEST-FILTER'"
  (when (string-match "Tests \\(FAILED\\|PASSED\\) - view reports" test-output)
    (let ((passed-p (equal "PASSED" (match-string-no-properties 1 test-output))))
      (beginning-of-buffer)
      (insert "\n-----\n")
      (if passed-p
          (insert (propertize "PASSED" 'face 'grails-test-passed))
        (insert (propertize "FAILED" 'face 'grails-test-failed)))
      (insert "\n")
      (insert "Some output files:\n")
      (insert-button "all-tests.html" 'action (lambda (but)
                                                (browse-url (project-append-to-path
                                                             (grails-tests-html-output-dir)
                                                             "all-tests.html"))))
      (dolist (file (grails-tests-list-of-plain-output-files))
        (insert "\n")
        (insert-button file 'action (lambda (but)
                                      (find-file (project-append-to-path
                                                  (grails-tests-plain-output-dir) (button-label but)))))
        (when (and (string-match "Tests?\\.txt$" file))
          (insert-file (project-append-to-path (grails-tests-plain-output-dir) file))))
      (insert "\n-----\n"))))

(defun grails-app-base-dir-for-file (file)
  (let ((parts (split-string file "[/\\\\]")))
    (block nil
      (while (setq parts (butlast parts))
        (let ((dir (concat (mapconcat 'identity parts "/") "/grails-app")))
          (when (file-exists-p dir)
            (return (mapconcat 'identity parts "/"))))))))

(defun grails-tests-html-output-dir nil
  (project-append-to-path
   (second (assoc :base-dir (grails-project-get-last-test)))
   '("target" "test-reports" "html")))

(defun grails-tests-plain-output-dir nil
  (project-append-to-path
   (second (assoc :base-dir (grails-project-get-last-test)))
   '("target" "test-reports" "plain")))

(defun grails-tests-list-of-plain-output-files nil
  (let (result)
    (dolist (file (directory-files (grails-tests-plain-output-dir)))
      (message file)
      (when (not (string-match "^\\." file))
        (setq result (append result (list file)))))
    result))

(defun grails-find-view-for-controller-action-at-point (buf)
  (save-excursion
    (set-buffer buf)
    (when (re-search-backward "def[ 	\n\r]+\\([a-z0-9]+\\)[ 	\n\r]*=[ 	\n\r]*{" (point-min) t)
      (let ((action-name (match-string-no-properties 1))
            (view-dir (downcase (substring (buffer-name) 0 (string-match "Controller\\." (buffer-name))))))
        (let ((file-path (project-append-to-path
                          (grails-app-base-dir-for-file (buffer-file-name buf))
                          (list "grails-app" "views"
                                view-dir
                                (concat action-name ".gsp")))))
          (if (file-readable-p file-path)
              (find-file file-path)
            (message (concat "Grails view '" view-dir "/" action-name "' doesn't exist."))))))))

(defun grails-find-file-for-stacktrace-line nil
  (interactive)
  (let ((bound (progn
                 (save-excursion
                   (end-of-line)
                   (point))))
        line-num
        file-name)
    (save-excursion
      (setq line-num (progn
                       (beginning-of-line)
                       (if (re-search-forward ":\\([0-9]+\\)" bound t)
                           (string-to-number (match-string-no-properties 1))
                         0)))
      (setq file-name (progn
                        (beginning-of-line)
                        (when (re-search-forward "(\\(.*+?\\)[:)]" bound t)
                          (match-string-no-properties 1)))))
    (when file-name
      (let (matches)
        (dolist (file (project-path-cache-get (project-current)))
          (when (and (or (not (string-match "Tests?\\.groovy$" file))
                         (equal (second (assoc :file (grails-project-get-last-test))) file))
                     (string-match (concat "[/\\\\]" file-name) file))
            (setq matches (append matches (list file)))))
        (when (and matches (file-readable-p (car matches)))
          (message (concat "Found: " (car matches) ":" (number-to-string line-num)))
          (find-file-other-window (car matches))
          (goto-line line-num)
          (beginning-of-line)
          (push-mark (point) t t)
          (end-of-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu
;; Refresh
(defun grails-mode-menu nil
  (interactive)
  (if grails-mode
      (progn
        (define-key-after
          global-map
          [menu-bar grailmenu]
          (cons "Grails" (make-sparse-keymap))
          'tools)

        (define-key
          global-map
          [menu-bar grailmenu ofjst]
          '("Find File For Stack-trace Line" . grails-find-file-for-stacktrace-line))

        (define-key
          global-map
          [menu-bar grailmenu svc]
          '("Find Service" . grails-find-service))

        (define-key
          global-map
          [menu-bar grailmenu contr]
          '("Find Controller" . grails-find-controller))

        (define-key
          global-map
          [menu-bar grailmenu domain]
          '("Find Domain" . grails-find-domain))

        (define-key
          global-map
          [menu-bar grailmenu runtest]
          '("Run Last Test" . grails-run-last-test))

        (define-key
          global-map
          [menu-bar grailmenu reruntest]
          '("Rerun Last Test" . grails-rerun-last-test))

        (define-key
          global-map
          [menu-bar grailmenu runinteg4c]
          '("Run Integration Test For Current" . grails-run-integration-test-for-current))

        (define-key
          global-map
          [menu-bar grailmenu rununit4c]
          '("Run Unit Test For Current" . grails-run-unit-test-for-current))

        (define-key
          global-map
          [menu-bar grailmenu integ4c]
          '("Find Integration Test For Current" . grails-find-integration-test-for-current))

        (define-key
          global-map
          [menu-bar grailmenu unit4c]
          '("Find Unit Test For Current" . grails-find-unit-test-for-current))

        (define-key
          global-map
          [menu-bar grailmenu svc4c]
          '("Find Service For Current" . grails-find-service-for-current))

        (define-key
          global-map
          [menu-bar grailmenu view4action]
          '("Find View For Action" . grails-find-view-for-controller-action))

        (define-key
          global-map
          [menu-bar grailmenu contr4c]
          '("Find Controller For Current" . grails-find-controller-for-current))

        (define-key
          global-map
          [menu-bar grailmenu domain4c]
          '("Find Domain For Current" . grails-find-domain-for-current)))
    (progn
      (global-unset-key [menu-bar grailmenu])))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'grails-mode)
