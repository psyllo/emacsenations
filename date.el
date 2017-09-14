(require 'org)
(require 'dash)

(defun date-part (date part)
  "`DATE' argument needs to be in this format MM/DD/YYYY."
  (if (equal 'month part)
      (first (split-string date "/"))
    (if (equal 'day part)
        (second (split-string date "/"))
      (if (equal 'year part)
          (third (split-string date "/"))))))

(defun date-range-part (date-range start-end part)
  "`DATE-RANGE' should be a list with two strings that are dates.
`START-END' should be a symbol 'END or 'START.
Date arguments need to be in this format MM/DD/YYYY."
  (let ((date (if (eq 'start start-end)
                  (first date-range)
                (second date-range))))
    (date-part date part)))

(defun date-to-sortable-number (date)
  "Convert MM/DD/YYYY to YYYYMMDD and then to an integer."
  (string-to-number (concat (date-part date 'year)
                            (date-part date 'month)
                            (date-part date 'day))))

(defun date-range-contains? (date-range date)
  (let ((start (date-to-sortable-number (first date-range)))
        (end (date-to-sortable-number (second date-range)))
        (date (date-to-sortable-number date)))
    (and (<= start date)
         (>= end date))))

(defun hms-to-seconds (hour min sec)
  "`hour` should be an integer 0-23
   `min` should be an integer 0-59
   `sec` should be an integer 0-59"
  (+ (* hour 60 60)
     (* min 60)
     sec))

(defun seconds-to-hms (seconds)
  "(seconds-to-hms (hms-to-seconds 9 45 0)) => (list 9 45 0)"
  (let* ((hours (/ seconds (* 60 60)))
         (mins (/ (- seconds (* hours 60 60)) 60))
         (secs (- seconds (* hours 60 60) (* mins 60))))
    (list hours mins secs)))

(defun seconds-from-midnight ()
  "Returns float of current time seconds from midnight"
  (- (float-time) (org-time-today)))

(defun hours-from-midnight ()
  (/ (seconds-from-midnight) (* 60.0 60)))

(defun hm-ampm-to-seconds (hhmm-ampm)
  "Accepts string like 1:45pm"
  (let* ((hhmm (mapcar 'string-to-int (split-string hhmm-ampm ":")))
         (hour (first hhmm))
         (min (second hhmm))
         (ampm (substring hhmm-ampm (- (length hhmm-ampm) 2))))
    (if (eq hour 12)
        (if (equal "am" ampm)
            (hms-to-seconds 0 min 0)
          (hms-to-seconds hour min 0))
      (+ (hms-to-seconds hour min 0)
         (if (equal "pm" ampm)
             (* 12 60 60)
           0)))))

(provide 'benfu-date)
