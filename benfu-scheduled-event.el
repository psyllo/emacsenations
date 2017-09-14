;; Synopsis: Run a function at scheduled times
;; Usage:
;; (add-to-list 'load-path "~/proj/emacsenations")
;; (require 'benfu-scheduled-event "benfu-scheduled-event.el")
;; (setq midnight-hook nil)
;; (benfu-scheduled-event t)
;; (setq benfu-scheduled-event-function-to-call 'spacemacs/cycle-spacemacs-theme)

(require 'midnight)
(require 'benfu-date "date.el")

;; Customize these values
(defvar benfu-scheduled-event-function-to-call (lambda () (message "benfu-scheduled-event fired its event"))
  "This is the customizable function to run at the given times.")
(setq benfu-scheduled-event-times (-cycle (list "12:00pm" "1:59pm" "3:00pm")))

(defun benfu-scheduled-event-pop-time ()
  (let ((result (first benfu-scheduled-event-times)))
    (setq benfu-scheduled-event-times (rest benfu-scheduled-event-times))
    result))

(defun went-back-in-time-p (time-string-1 time-string-2)
  "Input should be like '1:45pm'"
  (> (hm-ampm-to-seconds time-string-1) (hm-ampm-to-seconds time-string-2)))

(defun benfu-scheduled-event-enqueue ()
  (let* ((prev-time (or midnight-delay "12:00am"))
         (next-time (first benfu-scheduled-event-times)))
    (if (went-back-in-time-p prev-time next-time)
        (run-at-time "12:00am" nil 'benfu-scheduled-event-enqueue)
      (midnight-delay-set 'midnight-delay (benfu-scheduled-event-pop-time)))))

(defun very-close-to-p (seconds1 seconds2)
  (< (- seconds1 2) seconds2))

(defun benfu-scheduled-event-event-handler ()
  (let ((curr-time (seconds-from-midnight))
        (target-time (hm-ampm-to-seconds midnight-delay)))
    (when (very-close-to-p curr-time target-time)
      (funcall benfu-scheduled-event-function-to-call))
    (benfu-scheduled-event-enqueue)))

(defun benfu-scheduled-event-enable ()
  (midnight-mode t)
  (midnight-delay-set 'midnight-delay (benfu-scheduled-event-pop-time))
  (add-hook 'midnight-hook 'benfu-scheduled-event-event-handler))

(defun benfu-scheduled-event-disable ()
  (remove-hook 'midnight-hook 'benfu-scheduled-event-event-handler))

(defun benfu-scheduled-event (t-or-nil)
  (if t-or-nil
      (benfu-scheduled-event-enable)
    (benfu-scheduled-event-disable)))

;; (benfu-scheduled-event-enable)
;; (benfu-scheduled-event-disable)
;;; clear all hooks hook
;; (midnight-delay-set 'midnight-delay (benfu-scheduled-event-pop-time))
;; (midnight-delay-set 'midnight-delay "11:45pm")
;; (run-mode-hooks 'midnight-hook)

(provide 'benfu-scheduled-event)
