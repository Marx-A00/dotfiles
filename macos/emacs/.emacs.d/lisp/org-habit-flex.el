;;; org-habit-flex.el --- Weekday-specific habit tracking for org-habit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcos Andrade

;; Author: Marcos Andrade
;; Keywords: calendar, org
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; org-habit-flex adds weekday-specific scheduling to org-habit via the
;; :HABIT_WEEKDAYS: property.  Set it to space-separated ISO weekday
;; numbers (1=Monday .. 7=Sunday) and non-applicable days appear gray
;; in the consistency graph, with completions on those days filtered out.
;;
;; Example:
;;   * TODO Morning standup
;;     SCHEDULED: <2026-01-06 Mon .+1d>
;;     :PROPERTIES:
;;     :STYLE:    habit
;;     :HABIT_WEEKDAYS: 1 2 3 4 5
;;     :END:

;;; Code:

(require 'org)
(require 'org-habit)
(require 'cl-lib)

;;;; Customization

(defgroup org-habit-flex nil
  "Weekday-specific habit tracking for org-habit."
  :group 'org-habit)

(defcustom org-habit-flex-weekdays-property "HABIT_WEEKDAYS"
  "Property name for weekday restrictions.
Value: space-separated integers 1-7 (ISO weekday, 1=Monday, 7=Sunday)."
  :type 'string
  :group 'org-habit-flex)

;;;; Faces

(defface org-habit-flex-skip-face
  '((((background light)) (:background "#d0d0d8"))
    (((background dark))  (:background "#555566")))
  "Face for days when a habit is not applicable."
  :group 'org-habit-flex
  :group 'org-faces)

(defface org-habit-flex-skip-future-face
  '((((background light)) (:background "#e0e0e8"))
    (((background dark))  (:background "#444455")))
  "Face for future days when a habit is not applicable."
  :group 'org-habit-flex
  :group 'org-faces)

;;;; Core weekday logic

(defun org-habit-flex-parse-weekdays (str)
  "Parse STR into a list of ISO weekday integers (1-7).
STR is a space-separated string like \"1 2 3 4 5\".
Returns nil if STR is nil or empty.  Filters out invalid values."
  (when (and str (not (string-empty-p (string-trim str))))
    (cl-remove-if-not
     (lambda (n) (and (integerp n) (<= 1 n) (<= n 7)))
     (mapcar #'string-to-number (split-string (string-trim str))))))

(defun org-habit-flex-get-weekdays (&optional pom)
  "Get weekday restriction list for habit at POM.
Returns list of ISO weekday integers (1-7), or nil if no restriction."
  (org-habit-flex-parse-weekdays
   (org-entry-get (or pom (point)) org-habit-flex-weekdays-property)))

(defun org-habit-flex--epoch-day-to-iso-dow (day)
  "Convert DAY (absolute day number) to ISO day-of-week (1=Mon, 7=Sun).
Uses `calendar-day-of-week' which returns 0=Sunday."
  (let ((cal-dow (calendar-day-of-week (calendar-gregorian-from-absolute day))))
    (if (= cal-dow 0) 7 cal-dow)))

(defun org-habit-flex-day-applicable-p (day weekdays)
  "Return non-nil if DAY falls on one of WEEKDAYS.
DAY is days-since-epoch.  WEEKDAYS is a list of ISO weekday integers (1-7).
If WEEKDAYS is nil, all days are applicable."
  (or (null weekdays)
      (memq (org-habit-flex--epoch-day-to-iso-dow day) weekdays)))

;;;; Phase 2: Filter completion dates

(defun org-habit-flex--filter-done-dates (habit-data)
  "Filter HABIT-DATA done-dates to only applicable weekdays.
Intended as :filter-return advice for `org-habit-parse-todo'."
  (when habit-data
    (let ((weekdays (org-habit-flex-get-weekdays)))
      (when weekdays
        (setf (nth 4 habit-data)
              (cl-remove-if-not
               (lambda (day) (org-habit-flex-day-applicable-p day weekdays))
               (nth 4 habit-data)))))
    habit-data))

;;;; Phase 3 & 4: Graph rendering with skip face

(defvar-local org-habit-flex--current-weekdays nil
  "Weekdays for the habit currently being rendered.
Dynamically bound during `org-habit-build-graph'.")

(defun org-habit-flex--build-graph-advice (orig-fn habit starting current ending)
  "Advice around `org-habit-build-graph' to inject weekday context.
Reads weekday data from index 6 of HABIT, appended by parse-todo advice."
  (let ((org-habit-flex--current-weekdays (nth 6 habit)))
    (funcall orig-fn habit starting current ending)))

(defun org-habit-flex--get-faces-advice (orig-fn habit &optional now-days scheduled-days donep)
  "Advice around `org-habit-get-faces' to return skip face for non-applicable days.
When NOW-DAYS falls on a non-applicable weekday, returns the skip face pair."
  (let ((m-days (or now-days (time-to-days nil))))
    (if (and org-habit-flex--current-weekdays
             (not (org-habit-flex-day-applicable-p
                   m-days org-habit-flex--current-weekdays)))
        '(org-habit-flex-skip-face . org-habit-flex-skip-future-face)
      (funcall orig-fn habit now-days scheduled-days donep))))

;;;; Phase 5: Store weekdays in habit text property

(defun org-habit-flex--parse-todo-advice (orig-fn &optional pom)
  "Advice around `org-habit-parse-todo' to attach weekday data.
Stores the weekday list as a text property on the habit data list,
so it's available during graph rendering."
  (let* ((weekdays (org-habit-flex-get-weekdays pom))
         (result (funcall orig-fn pom)))
    (when (and result weekdays)
      ;; Filter done-dates to applicable weekdays
      (setf (nth 4 result)
            (cl-remove-if-not
             (lambda (day) (org-habit-flex-day-applicable-p day weekdays))
             (nth 4 result)))
      ;; Append weekdays as index 6 so build-graph can read them.
      ;; org-habit only accesses indices 0-5, so this is safe.
      (setcdr (last result) (list weekdays)))
    result))

;;;; Phase 6: Reschedule to next applicable weekday

(defun org-habit-flex--next-applicable-day (day weekdays)
  "Return the next day on or after DAY that falls on one of WEEKDAYS.
DAY is days-since-epoch.  WEEKDAYS is a list of ISO weekday integers (1-7).
If DAY itself is applicable, returns DAY."
  (if (or (null weekdays)
          (org-habit-flex-day-applicable-p day weekdays))
      day
    (let ((d (1+ day)))
      ;; At most 7 iterations to find the next applicable day
      (while (not (org-habit-flex-day-applicable-p d weekdays))
        (cl-incf d))
      d)))

(defun org-habit-flex--adjust-schedule ()
  "Adjust SCHEDULED date to the next applicable weekday after repeat.
Intended for use in `org-todo-repeat-hook'."
  (when (and org-habit-flex--active
             (org-is-habit-p))
    (let ((weekdays (org-habit-flex-get-weekdays)))
      (when weekdays
        (let* ((scheduled (org-entry-get nil "SCHEDULED"))
               (time (and scheduled (org-time-string-to-time scheduled)))
               (day (and time (time-to-days time))))
          (when (and day (not (org-habit-flex-day-applicable-p day weekdays)))
            (let* ((next-day (org-habit-flex--next-applicable-day day weekdays))
                   (delta (- next-day day)))
              (save-excursion
                (org-back-to-heading t)
                (when (re-search-forward org-scheduled-time-regexp
                                         (org-entry-end-position) t)
                  (goto-char (match-beginning 1))
                  (org-timestamp-change delta 'day))))))))))

;;;; Phase 7: Hide habits on non-applicable agenda days

(defvar org-habit-flex--rendering-date nil
  "The agenda date currently being rendered by `org-agenda-get-scheduled'.
Bound dynamically via advice; nil outside that context.")

(defun org-habit-flex--get-scheduled-advice (orig-fn &rest args)
  "Advice around `org-agenda-get-scheduled' to bind the rendering date.
Reads the dynamically-scoped `date' variable set by the agenda caller.
Also disables `org-habit-show-habits-only-for-today' so the built-in
habit filter doesn't suppress entries before our skip function runs."
  (with-no-warnings (defvar date))
  (let ((org-habit-flex--rendering-date date)
        (org-habit-show-habits-only-for-today nil))
    (apply orig-fn args)))

(defun org-habit-flex-agenda-skip ()
  "Skip habit entry if the rendering date is not an applicable weekday.
Returns `org-entry-end-position' to skip, or nil to keep.
Only active inside `org-agenda-get-scheduled' where the date context is reliable."
  (and org-habit-flex--rendering-date
       (org-is-habit-p)
       (let ((weekdays (org-habit-flex-get-weekdays)))
         (when weekdays
           (let ((day (calendar-absolute-from-gregorian org-habit-flex--rendering-date)))
             (unless (org-habit-flex-day-applicable-p day weekdays)
               (org-entry-end-position)))))))

;;;; Activation

(defvar org-habit-flex--active nil
  "Non-nil when org-habit-flex advice is active.")

;;;###autoload
(defun org-habit-flex-activate ()
  "Activate org-habit-flex weekday support."
  (interactive)
  (unless org-habit-flex--active
    (advice-add 'org-habit-parse-todo :around #'org-habit-flex--parse-todo-advice)
    (advice-add 'org-habit-build-graph :around #'org-habit-flex--build-graph-advice)
    (advice-add 'org-habit-get-faces :around #'org-habit-flex--get-faces-advice)
    (add-hook 'org-todo-repeat-hook #'org-habit-flex--adjust-schedule)
    (advice-add 'org-agenda-get-scheduled :around #'org-habit-flex--get-scheduled-advice)
    (setq org-agenda-skip-function-global #'org-habit-flex-agenda-skip)
    (setq org-habit-flex--active t)
    (message "org-habit-flex activated")))

;;;###autoload
(defun org-habit-flex-deactivate ()
  "Deactivate org-habit-flex weekday support."
  (interactive)
  (when org-habit-flex--active
    (advice-remove 'org-habit-parse-todo #'org-habit-flex--parse-todo-advice)
    (advice-remove 'org-habit-build-graph #'org-habit-flex--build-graph-advice)
    (advice-remove 'org-habit-get-faces #'org-habit-flex--get-faces-advice)
    (remove-hook 'org-todo-repeat-hook #'org-habit-flex--adjust-schedule)
    (advice-remove 'org-agenda-get-scheduled #'org-habit-flex--get-scheduled-advice)
    (setq org-agenda-skip-function-global nil)
    (setq org-habit-flex--active nil)
    (message "org-habit-flex deactivated")))

(provide 'org-habit-flex)

;;; org-habit-flex.el ends here
