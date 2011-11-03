;;; clojure-project-mode.el
;;
;; Author: Benjamin Cluff
;; Created: 03-Nov-2011
;;
;; Synopsis: Extends project-mode for Clojure specific needs.
;;

(require 'project-mode)

;;;###autoload

(setq project-tags-form-default
           '(".clj$"
             ('elisp "(def[^
	\"#]+")))

(provide 'clojure-project-mode)