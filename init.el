(defun pgw/date-block (absolute y1 m1 d1 y2 m2 d2)
  "Block date entry. An adapted version of the `diary-block'
function from the diary-lib."
  (let ((date1 (calendar-absolute-from-gregorian
                (diary-make-date y1 m1 d1)))
        (date2 (calendar-absolute-from-gregorian
                (diary-make-date y2 m2 d2)))
        (d absolute))
    (and (<= date1 d) (<= d date2))))

(defun pgw/date-date (absolute year month day)
  "Check for equality of date"
  (equal absolute (calendar-absolute-from-gregorian (diary-make-date year month day))))

(defun pgw/check-ohs-class (absolute classname days period)
  "Returns a list with formatted strings: (classname curdate
headline). These can then be used to create the headline. The curdate
is in the form of a list"
  (let ((dayname (calendar-day-of-week (calendar-gregorian-from-absolute absolute)))
        (curdate (calendar-gregorian-from-absolute absolute))
        (time nil))
    (cond ((equal period 1) (setq time "06:00-07:15"))
          ((equal period 2) (setq time "07:15-08:30"))
          ((equal period 3) (setq time "08:30-09:45"))
          ((equal period 4) (setq time "09:45-11:00"))
          ((equal period 5) (setq time "11:00-12:15"))
          ((equal period 6) (setq time "12:15-13:30"))
          ((equal period 7) (setq time "13:30-14:45"))
          ((equal period 8) (setq time "14:45-16:00"))
          ((equal period 9) (setq time "16:00-17:15"))
          ((equal period 10) (setq time "17:15-18:30"))
          ((equal period 11) (setq time "18:30-19:45"))
          ((equal period 12) (setq time "19:45-21:00"))
          ((equal period 13) (setq time "21:00-22:15")))
    (when (and (cond ((equal days '(1 3)) (or (memq dayname '(1 3)) (pgw/date-date absolute 2021 1 22)))
                     (t (memq dayname days)))
               (pgw/date-block absolute 2020 8 19 2021 5 13)) ;; Class Period
      (when (not (or (pgw/date-date absolute 2020 9 7) ;; Labor Day
                     (pgw/date-date absolute 2020 9 11) ;; Back to School Night
                     (pgw/date-block absolute 2020 10 28 2020 10 30) ;; Parent-Teacher Conferences (no classes)
                     (pgw/date-block absolute 2020 11 25 2020 11 27) ;; Thanksgiving Holiday
                     (pgw/date-block absolute 2020 12 9 2020 12 11) ;; Study Days (no classes)
                     (pgw/date-block absolute 2020 12 14 2020 12 19) ;; Fall Semester Finals
                     (pgw/date-block absolute 2020 12 19 2021 1 3) ;; Winter Closure
                     (pgw/date-block absolute 2021 1 4 2021 1 8) ;; Reading Week
                     (pgw/date-date absolute 2021 1 18) ;; MLK Holiday
                     (pgw/date-date absolute 2021 2 15) ;; Presidents Day
                     (pgw/date-date absolute 2021 2 16) ;; Reading Day (No classes)
                     (pgw/date-block absolute 2021 3 22 2021 3 26) ;; Spring Break
                     (pgw/date-block absolute 2021 5 17 2021 5 19) ;; Study Days
                     (pgw/date-block absolute 2021 5 20 2021 5 21) ;; Spring Semester Finals
                     (pgw/date-block absolute 2021 5 24 2021 5 27) ;; Spring Semester Finals
                     (pgw/date-date absolute 2021 5 31))) ;; Memorial Day Holiday
        (list classname curdate time)))))

(defun pgw/create-class (classname days period)
  "Creates headlines for class schedule First date is start of
school. Second date is end of school. classname is the name of the
class (to display as headline) days is an integer, either 1, 2 or
3. The three integers represent M/W, T/Th, and F respectively"
  (let ((current (calendar-absolute-from-gregorian (diary-make-date 2020 8 19))))
    (while (pgw/date-block current 2020 8 19 2021 5 13) ; Make sure we're within starting and ending dates of school
      (let ((info (pgw/check-ohs-class current classname days period)))
        (when info
          (let* ((headline (nth 0 info))
                 (days-of-week '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
                 (fulldate (nth 1 info))
                 (year (nth 2 fulldate))
                 (month (nth 0 fulldate))
                 (day (nth 1 fulldate))
                 (dayofweek (nth (calendar-day-of-week fulldate) days-of-week))
                 (time (nth 2 info)))
            (goto-char (point-max))
            (insert (format "\n* %s\n:PROPERTIES:\n:TIMEZONE: UTC\n:END:\n:org-gcal:\n<%d-%02d-%02d %s %s>\n:END:"
                            headline year month day dayofweek time)))))
      (setq current (+ current 1))
      (message (format "Current count: %s" current)))))
