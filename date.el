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
