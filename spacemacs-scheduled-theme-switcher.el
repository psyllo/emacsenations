;; Synopsis: Change theme at lunch and at tea-time. A final time of just before midnight
;; to have a stopping point.

(require 'midnight)
(require 'benfu-date "date.el")

;; An order sequence of times at which to change the theme.
;; Customize these values
(setq benfu-change-theme-times (-cycle (list "12:00pm" "3:00pm")))

(defun benfu-change-theme-pop-time ()
  (let ((result (first benfu-change-theme-times)))
    (setq benfu-change-theme-times (rest benfu-change-theme-times))
    result))

(defun went-back-in-time-p (time-string-1 time-string-2)
  "Input should be like '1:45pm'"
  (> (hm-ampm-to-seconds time-string-1) (hm-ampm-to-seconds time-string-2)))

(defun benfu-change-theme-enqueue ()
  (let* ((prev-time (or midnight-delay "12:00am"))
         (next-time (first benfu-change-theme-times)))
    (if (went-back-in-time-p prev-time next-time)
        (run-at-time "12:00am" nil 'benfu-change-theme-enqueue)
      (midnight-delay-set 'midnight-delay (benfu-change-theme-pop-time)))))

(defun very-close-to-p (seconds1 seconds2)
  (< (- seconds1 2) seconds2))

(defun benfu-change-theme ()
  (let ((curr-time (seconds-from-midnight))
        (target-time (hm-ampm-to-seconds midnight-delay)))
    (when (very-close-to-p curr-time target-time)
      (spacemacs/cycle-spacemacs-theme)
      (benfu-change-theme-enqueue))))

(defun benfu-change-theme-enable ()
  (midnight-mode t)
  (setq midnight-hook nil)
  (midnight-delay-set 'midnight-delay (benfu-change-theme-pop-time))
  (add-hook 'midnight-hook 'benfu-change-theme))

(defun benfu-change-theme-disable ()
  (remove-hook 'midnight-hook 'benfu-change-theme))

;; (benfu-change-theme-enable)
;; (benfu-change-theme-disable)
;;; clear all hooks hook
;; (midnight-delay-set 'midnight-delay (benfu-change-theme-pop-time))
;; (midnight-delay-set 'midnight-delay "11:45pm")
;; (run-mode-hooks 'midnight-hook)

(provide 'benfu-spacemacs-scheduled-theme-switcher)
