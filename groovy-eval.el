;;; groovy-eval.el
;;
;; Author: Benjamin Cluff
;; Created: 27-Jan-2010
;;
;; Synopsis: Evaluate groovy code in emacs
;; - Executes system command 'groovy -l' to open a server socket
;; - Connects to server by opening a socket in emacs
;; - The user can execute multi-line regions of groovy code with the bound key strokes
;; - The string is pre-processed to collapse it to a single valid line of groovy code
;; - The string is sent to the client process
;; - Output and result are displayed
;;
;; Usage:
;;   (autoload 'groovy-eval "groovy-eval" "Groovy Evaluation" t)
;;   (add-hook 'groovy-mode-hook 'groovy-eval)

;;
;; Known bugs/limitations:
;; - Multi-line strings (i.e. """ or ''' are ignored and the contents are
;;   collapsed to a single line with semicolons where the new lines were.


(defvar groovy-eval-server-port (string-to-number (concat (number-to-string 9)
                                                          (number-to-string (random* 9))
                                                          (number-to-string (random* 9))
                                                          (number-to-string (random* 9))))
  "Port used for groovy-eval.") ; Somewhat randomized to reduce the chances of other emacs instances using the same port

(defvar groovy-eval-buffer-name "*groovy-eval*")

(defun groovy-eval-stop nil
  (interactive)
  (condition-case nil
      (progn
        (kill-process (get-process "groovy-eval-client"))
        (kill-process (get-process "groovy-eval-server")))
    (error nil))
  (when (get-buffer groovy-eval-buffer-name)
    (kill-buffer groovy-eval-buffer-name)))

(defun groovy-eval-list-processes nil
  (interactive)
  (list-processes)) ; TODO show just relevant processes

(defun groovy-eval-filter (proc str)
  (if (string-match "is listening on port" str)
      (groovy-eval-client-start) ; Start client since server is listening (TODO: Potential for a hook here)
    (progn
      (save-excursion
        (set-buffer groovy-eval-buffer-name)
        (goto-char (point-max))
        (insert (concat "\n" str)))
      (save-excursion
        (message str)))))

(defun groovy-eval-start nil
  "starts a groovy server"
  (interactive)
  (if (featurep 'make-network-process '(:server t))
      (if (process-status "groovy-eval-server")
          (message (concat "Groovy running on port " (number-to-string groovy-eval-server-port)))
        (let ((proc (start-process-shell-command "groovy-eval-server"
                                                 (get-buffer-create groovy-eval-buffer-name)
                                                 "groovy" "-l" (number-to-string groovy-eval-server-port) "-p" "-e"
                                                 "\"try{Eval.me(line)}catch(Exception e){println e.printStackTrace()}\"")))
          (set-process-filter proc (lambda (proc str) (groovy-eval-filter proc (concat "Output=> " str))))
          (process-kill-without-query proc)))
    (message "Sorry. I can't create a server. Trying creating one yourself and just using the groovy-eval-client-start.")))

(defun groovy-eval-client-start nil
  "starts a groovy client"
  (interactive)
  (if (process-status "groovy-eval-client")
      (message "Groovy client already started")
    (let ((proc (make-network-process :name "groovy-eval-client"
                                      :buffer (get-buffer-create groovy-eval-buffer-name)
                                      :host "localhost" :service groovy-eval-server-port
                                      :family 'ipv4 :server nil
                                      :filter (lambda (proc str) (groovy-eval-filter proc (concat "Result=> " str))))))
      (progn
        (message "groovy-eval is ready (hint: use 'C-x C-e' to evaluate a region)")
        (process-kill-without-query proc)))))

(defun groovy-eval-code-to-single-line (groovy-code)
  (replace-regexp-in-string ; replace new lines with semicolons
   "\n" "; "
   (replace-regexp-in-string ; remove empty lines
    "^[ \t]*\n" ""
    (replace-regexp-in-string ; collapse a dangling list terminator "]"
     "\n[ \t]*\\]" "]"
     (replace-regexp-in-string ; collapse some multi-line expressions. (assign "=", concat "+", lists "[", list contents ",")
      "\\([^+][=+,\\[]\\)[ \t]*\n" "\\1 "
      (replace-regexp-in-string
       "}[\s\t\r\n]*catch[\s\t\r\n]*([\s\t\r\n]*\\(.*?\\)[\s\t\r\n]*)[\s\t\r\n]*{" "} catch(\\1) {"
      (replace-regexp-in-string ; remove comments at the end of a line of code
       "[ \t]*[^:]//.*" ""
       (replace-regexp-in-string ; remove comment on a line by themselves
        "^[ \t]*[^:]//.*\n" ""
        groovy-code))))))))

(defun groovy-eval-execute-string (groovy-code)
  (if (process-status "groovy-eval-client")
   (process-send-string "groovy-eval-client"
       (concat
        (groovy-eval-code-to-single-line groovy-code)
        "\n"))
    (message "groovy-eval not started. Try 'M-x groovy-eval'")))

(defun groovy-eval-execute nil
  (interactive)
  (groovy-eval-execute-string (buffer-substring (region-beginning) (region-end))))

(defun groovy-eval-execute-buffer nil
  (interactive)
  (groovy-eval-execute-string (buffer-substring (point-min) (point-max))))

(defun groovy-eval-show-buffer (eob-p)
  "Switch to the groovy-eval buffer.
`EOB-P' positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer groovy-eval-buffer-name)
      (pop-to-buffer groovy-eval-buffer-name)
      (error "No process buffer found for groovy-eval."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun groovy-eval-keys nil
  (define-key groovy-mode-map [?\C-x ?\C-e] 'groovy-eval-execute)
  (define-key groovy-mode-map [?\C-|] 'groovy-eval-execute-buffer)
  (define-key groovy-mode-map [?\C-c ?\C-z] 'groovy-eval-show-buffer))

(defun groovy-eval nil
  (interactive)
  (groovy-eval-stop)
  (sit-for 1)
  (groovy-eval-start)
  (groovy-eval-keys))

(provide 'groovy-eval)
