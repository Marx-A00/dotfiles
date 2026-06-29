;;; org-habit-flex-test.el --- Tests for org-habit-flex -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for org-habit-flex.
;; Run with:
;;   emacs --batch -l org -l org-habit \
;;     -l ~/.emacs.d/lisp/org-habit-flex.el \
;;     -l ~/.emacs.d/tests/org-habit-flex-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-habit-flex)

;; Known absolute day numbers for test fixtures (2024-01-06 week):
;; 738891 = Saturday  2024-01-06 (ISO 6)
;; 738892 = Sunday    2024-01-07 (ISO 7)
;; 738893 = Monday    2024-01-08 (ISO 1)
;; 738894 = Tuesday   2024-01-09 (ISO 2)
;; 738895 = Wednesday 2024-01-10 (ISO 3)
;; 738896 = Thursday  2024-01-11 (ISO 4)
;; 738897 = Friday    2024-01-12 (ISO 5)

;;; =========================================================
;;; Phase 1: Core weekday logic
;;; =========================================================

(ert-deftest ohf-test-parse-weekdays-basic ()
  "Parse a normal weekday string."
  (should (equal '(1 2 3 4 5) (org-habit-flex-parse-weekdays "1 2 3 4 5"))))

(ert-deftest ohf-test-parse-weekdays-single ()
  "Parse a single weekday."
  (should (equal '(5) (org-habit-flex-parse-weekdays "5"))))

(ert-deftest ohf-test-parse-weekdays-weekend ()
  "Parse weekend days."
  (should (equal '(6 7) (org-habit-flex-parse-weekdays "6 7"))))

(ert-deftest ohf-test-parse-weekdays-nil ()
  "Nil input returns nil."
  (should (null (org-habit-flex-parse-weekdays nil))))

(ert-deftest ohf-test-parse-weekdays-empty ()
  "Empty string returns nil."
  (should (null (org-habit-flex-parse-weekdays "")))
  (should (null (org-habit-flex-parse-weekdays "   "))))

(ert-deftest ohf-test-parse-weekdays-filters-invalid ()
  "Out-of-range values are filtered."
  (should (equal '(1 5) (org-habit-flex-parse-weekdays "0 1 5 8 99"))))

(ert-deftest ohf-test-parse-weekdays-extra-spaces ()
  "Extra whitespace is handled."
  (should (equal '(1 3 5) (org-habit-flex-parse-weekdays "  1  3  5  "))))

;;; Day-of-week conversion

(ert-deftest ohf-test-epoch-day-to-iso-dow ()
  "Verify epoch-day to ISO day-of-week conversion."
  (should (= 6 (org-habit-flex--epoch-day-to-iso-dow 738891)))  ; Saturday
  (should (= 7 (org-habit-flex--epoch-day-to-iso-dow 738892)))  ; Sunday
  (should (= 1 (org-habit-flex--epoch-day-to-iso-dow 738893)))  ; Monday
  (should (= 2 (org-habit-flex--epoch-day-to-iso-dow 738894)))  ; Tuesday
  (should (= 3 (org-habit-flex--epoch-day-to-iso-dow 738895)))  ; Wednesday
  (should (= 4 (org-habit-flex--epoch-day-to-iso-dow 738896)))  ; Thursday
  (should (= 5 (org-habit-flex--epoch-day-to-iso-dow 738897)))) ; Friday

;;; Day applicability

(ert-deftest ohf-test-day-applicable-weekday-on-weekday ()
  "Monday is applicable for weekday-only habits."
  (should (org-habit-flex-day-applicable-p 738893 '(1 2 3 4 5))))

(ert-deftest ohf-test-day-applicable-weekend-on-weekday ()
  "Saturday is NOT applicable for weekday-only habits."
  (should-not (org-habit-flex-day-applicable-p 738891 '(1 2 3 4 5))))

(ert-deftest ohf-test-day-applicable-nil-weekdays ()
  "Nil weekdays means all days are applicable."
  (should (org-habit-flex-day-applicable-p 738891 nil))
  (should (org-habit-flex-day-applicable-p 738893 nil)))

(ert-deftest ohf-test-day-applicable-specific-days ()
  "MWF pattern: Mon/Wed/Fri applicable, others not."
  (let ((mwf '(1 3 5)))
    (should     (org-habit-flex-day-applicable-p 738893 mwf))  ; Mon
    (should-not (org-habit-flex-day-applicable-p 738894 mwf))  ; Tue
    (should     (org-habit-flex-day-applicable-p 738895 mwf))  ; Wed
    (should-not (org-habit-flex-day-applicable-p 738896 mwf))  ; Thu
    (should     (org-habit-flex-day-applicable-p 738897 mwf))  ; Fri
    (should-not (org-habit-flex-day-applicable-p 738891 mwf))  ; Sat
    (should-not (org-habit-flex-day-applicable-p 738892 mwf)))) ; Sun

(ert-deftest ohf-test-day-applicable-weekend-only ()
  "Weekend-only pattern."
  (let ((weekend '(6 7)))
    (should     (org-habit-flex-day-applicable-p 738891 weekend))  ; Sat
    (should     (org-habit-flex-day-applicable-p 738892 weekend))  ; Sun
    (should-not (org-habit-flex-day-applicable-p 738893 weekend)))) ; Mon

;;; =========================================================
;;; Phase 2: Filter completion dates
;;; =========================================================

(ert-deftest ohf-test-filter-done-dates ()
  "Done-dates are filtered to applicable weekdays only."
  ;; Simulate a habit with done-dates on Mon(738893), Tue(738894), Sat(738891)
  (let ((habit-data (list 738893 1 nil nil '(738893 738894 738891) ".+")))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n"
              "  SCHEDULED: <2024-01-06 Mon .+1d>\n"
              "  :PROPERTIES:\n"
              "  :STYLE: habit\n"
              "  :HABIT_WEEKDAYS: 1 2 3 4 5\n"
              "  :END:\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((filtered (org-habit-flex--filter-done-dates habit-data)))
        ;; Saturday (738891) should be filtered out
        (should (equal '(738893 738894) (nth 4 filtered)))))))

(ert-deftest ohf-test-filter-done-dates-no-weekdays ()
  "Without HABIT_WEEKDAYS, done-dates are unchanged."
  (let ((habit-data (list 738893 1 nil nil '(738893 738894 738891) ".+")))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n"
              "  SCHEDULED: <2024-01-06 Mon .+1d>\n"
              "  :PROPERTIES:\n"
              "  :STYLE: habit\n"
              "  :END:\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((filtered (org-habit-flex--filter-done-dates habit-data)))
        (should (equal '(738893 738894 738891) (nth 4 filtered)))))))

(ert-deftest ohf-test-filter-done-dates-nil-input ()
  "Nil habit-data passes through."
  (should (null (org-habit-flex--filter-done-dates nil))))

;;; =========================================================
;;; Phase 3: Skip face for non-applicable days
;;; =========================================================

(ert-deftest ohf-test-get-faces-skip-non-applicable ()
  "Non-applicable days get skip face."
  (let ((org-habit-flex--current-weekdays '(1 2 3 4 5))
        ;; Minimal habit data: scheduled=738893(Mon), repeat=1d, no deadline
        (habit (list 738893 1 nil nil '() ".+")))
    ;; Saturday (738891) is not applicable for weekday-only
    (let ((faces (org-habit-flex--get-faces-advice
                  #'org-habit-get-faces habit 738891 nil nil)))
      (should (eq 'org-habit-flex-skip-face (car faces)))
      (should (eq 'org-habit-flex-skip-future-face (cdr faces))))))

(ert-deftest ohf-test-get-faces-applicable-day-passes-through ()
  "Applicable days use normal org-habit faces."
  (let ((org-habit-flex--current-weekdays '(1 2 3 4 5))
        (habit (list 738893 1 nil nil '() ".+")))
    ;; Monday (738893) is applicable
    (let ((faces (org-habit-flex--get-faces-advice
                  #'org-habit-get-faces habit 738893 738893 nil)))
      ;; Should NOT be the skip face
      (should-not (eq 'org-habit-flex-skip-face (car faces))))))

(ert-deftest ohf-test-get-faces-no-weekdays-passes-through ()
  "Without weekday restrictions, all days pass through to original."
  (let ((org-habit-flex--current-weekdays nil)
        (habit (list 738893 1 nil nil '() ".+")))
    (let ((faces (org-habit-flex--get-faces-advice
                  #'org-habit-get-faces habit 738891 nil nil)))
      (should-not (eq 'org-habit-flex-skip-face (car faces))))))

;;; =========================================================
;;; Phase 4 & 5: Integration - parse-todo advice
;;; =========================================================

(ert-deftest ohf-test-parse-todo-advice-filters-dates ()
  "The parse-todo :around advice filters done-dates."
  ;; We test the advice function directly with a mock orig-fn
  (let* ((raw-habit (list 738893 1 nil nil '(738893 738894 738891) ".+"))
         (mock-orig (lambda (&optional _pom) raw-habit)))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n"
              "  SCHEDULED: <2024-01-06 Mon .+1d>\n"
              "  :PROPERTIES:\n"
              "  :STYLE: habit\n"
              "  :HABIT_WEEKDAYS: 1 2 3 4 5\n"
              "  :END:\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((result (org-habit-flex--parse-todo-advice mock-orig)))
        ;; Saturday (738891) filtered out
        (should (equal '(738893 738894) (nth 4 result)))
        ;; Weekdays appended at index 6
        (should (equal '(1 2 3 4 5) (nth 6 result)))))))

(ert-deftest ohf-test-parse-todo-advice-no-weekdays ()
  "Without HABIT_WEEKDAYS, parse-todo advice is a no-op."
  (let* ((raw-habit (list 738893 1 nil nil '(738893 738894 738891) ".+"))
         (mock-orig (lambda (&optional _pom) raw-habit)))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test habit\n"
              "  SCHEDULED: <2024-01-06 Mon .+1d>\n"
              "  :PROPERTIES:\n"
              "  :STYLE: habit\n"
              "  :END:\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((result (org-habit-flex--parse-todo-advice mock-orig)))
        (should (equal '(738893 738894 738891) (nth 4 result)))
        ;; No weekdays appended
        (should (null (nth 6 result)))))))

(ert-deftest ohf-test-build-graph-reads-weekdays-from-index-6 ()
  "Build-graph advice reads weekdays from index 6 of habit list."
  (let* ((called-weekdays nil)
         (habit (list 738893 1 nil nil '() ".+" '(1 3 5)))
         (mock-orig (lambda (_h _s _c _e)
                      (setq called-weekdays org-habit-flex--current-weekdays)
                      "mock-graph")))
    (should (equal "mock-graph"
                   (org-habit-flex--build-graph-advice mock-orig habit nil nil nil)))
    (should (equal '(1 3 5) called-weekdays))))

(ert-deftest ohf-test-build-graph-nil-weekdays-when-absent ()
  "Build-graph advice sets nil weekdays when index 6 is absent."
  (let* ((called-weekdays :not-set)
         (habit (list 738893 1 nil nil '() ".+"))
         (mock-orig (lambda (_h _s _c _e)
                      (setq called-weekdays org-habit-flex--current-weekdays)
                      "mock-graph")))
    (org-habit-flex--build-graph-advice mock-orig habit nil nil nil)
    (should (null called-weekdays))))

;;; =========================================================
;;; Phase 6: Reschedule to next applicable weekday
;;; =========================================================

(ert-deftest ohf-test-next-applicable-day-already-applicable ()
  "If the day is already applicable, return it unchanged."
  (should (= 738893 (org-habit-flex--next-applicable-day 738893 '(1 2 3 4 5)))))  ; Monday

(ert-deftest ohf-test-next-applicable-day-skip-weekend ()
  "Saturday with weekday-only should skip to Monday."
  ;; 738891=Sat, next Monday=738893
  (should (= 738893 (org-habit-flex--next-applicable-day 738891 '(1 2 3 4 5)))))

(ert-deftest ohf-test-next-applicable-day-skip-sunday ()
  "Sunday with weekday-only should skip to Monday."
  ;; 738892=Sun, next Monday=738893
  (should (= 738893 (org-habit-flex--next-applicable-day 738892 '(1 2 3 4 5)))))

(ert-deftest ohf-test-next-applicable-day-nil-weekdays ()
  "Nil weekdays means any day is applicable."
  (should (= 738891 (org-habit-flex--next-applicable-day 738891 nil))))

(ert-deftest ohf-test-next-applicable-day-mwf ()
  "MWF pattern: Tuesday should skip to Wednesday."
  ;; 738894=Tue, next Wed=738895
  (should (= 738895 (org-habit-flex--next-applicable-day 738894 '(1 3 5)))))

(ert-deftest ohf-test-next-applicable-day-mwf-thursday ()
  "MWF pattern: Thursday should skip to Friday."
  ;; 738896=Thu, next Fri=738897
  (should (= 738897 (org-habit-flex--next-applicable-day 738896 '(1 3 5)))))

(ert-deftest ohf-test-next-applicable-day-mwf-saturday ()
  "MWF pattern: Saturday should skip to Monday."
  ;; 738891=Sat, next Mon=738893
  (should (= 738893 (org-habit-flex--next-applicable-day 738891 '(1 3 5)))))

(ert-deftest ohf-test-adjust-schedule-skips-weekend ()
  "Rescheduling from Saturday jumps to Monday for weekday-only habits."
  (org-habit-flex-activate)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        ;; Saturday 2024-01-06 is actually a Saturday
        (insert "* TODO Test habit\n"
                "  SCHEDULED: <2024-01-06 Sat .+1d>\n"
                "  :PROPERTIES:\n"
                "  :STYLE: habit\n"
                "  :HABIT_WEEKDAYS: 1 2 3 4 5\n"
                "  :END:\n")
        (goto-char (point-min))
        (org-back-to-heading t)
        (org-habit-flex--adjust-schedule)
        ;; Should have moved to Monday 2024-01-08
        (let ((sched (org-entry-get nil "SCHEDULED")))
          (should (string-match-p "2024-01-08" sched))))
    (org-habit-flex-deactivate)))

(ert-deftest ohf-test-adjust-schedule-no-change-when-applicable ()
  "No adjustment when scheduled date is already on an applicable day."
  (org-habit-flex-activate)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        ;; 2024-01-08 is Monday
        (insert "* TODO Test habit\n"
                "  SCHEDULED: <2024-01-08 Mon .+1d>\n"
                "  :PROPERTIES:\n"
                "  :STYLE: habit\n"
                "  :HABIT_WEEKDAYS: 1 2 3 4 5\n"
                "  :END:\n")
        (goto-char (point-min))
        (org-back-to-heading t)
        (org-habit-flex--adjust-schedule)
        (let ((sched (org-entry-get nil "SCHEDULED")))
          (should (string-match-p "2024-01-08" sched))))
    (org-habit-flex-deactivate)))

(ert-deftest ohf-test-adjust-schedule-no-weekdays-no-change ()
  "Without HABIT_WEEKDAYS, no adjustment is made."
  (org-habit-flex-activate)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test habit\n"
                "  SCHEDULED: <2024-01-06 Sat .+1d>\n"
                "  :PROPERTIES:\n"
                "  :STYLE: habit\n"
                "  :END:\n")
        (goto-char (point-min))
        (org-back-to-heading t)
        (org-habit-flex--adjust-schedule)
        (let ((sched (org-entry-get nil "SCHEDULED")))
          (should (string-match-p "2024-01-06" sched))))
    (org-habit-flex-deactivate)))

;;; =========================================================
;;; Phase 7: Agenda skip on non-applicable days
;;; =========================================================

;; Helper to build a habit buffer and run the skip function with a given agenda date.
;; RENDERING-DATE is (month day year) as org-agenda uses, or nil.
(defun ohf-test--run-skip (rendering-date habit-weekdays-prop &optional non-habit)
  "Run `org-habit-flex-agenda-skip' in a temp buffer.
RENDERING-DATE is (month day year) simulating the date from get-scheduled advice.
HABIT-WEEKDAYS-PROP is the property string.
If NON-HABIT is non-nil, omit the STYLE property."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test habit\n"
            "  SCHEDULED: <2024-01-08 Mon .+1d>\n"
            "  :PROPERTIES:\n"
            (if non-habit "" "  :STYLE: habit\n")
            (if habit-weekdays-prop
                (format "  :HABIT_WEEKDAYS: %s\n" habit-weekdays-prop)
              "")
            "  :END:\n")
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((org-habit-flex--rendering-date rendering-date))
      (org-habit-flex-agenda-skip))))

(ert-deftest ohf-test-skip-on-off-day ()
  "Habit is skipped when agenda date is a non-applicable weekday."
  ;; Wednesday 2024-01-10 (ISO 3), weekdays 1 2 4 5 (no Wed)
  (should (ohf-test--run-skip '(1 10 2024) "1 2 4 5")))

(ert-deftest ohf-test-skip-keep-on-applicable-day ()
  "Habit is kept when agenda date is an applicable weekday."
  ;; Thursday 2024-01-11 (ISO 4), weekdays 1 2 4 5
  (should-not (ohf-test--run-skip '(1 11 2024) "1 2 4 5")))

(ert-deftest ohf-test-skip-keep-non-habit ()
  "Non-habit entries are never skipped."
  ;; Wednesday, off-day, but not a habit
  (should-not (ohf-test--run-skip '(1 10 2024) "1 2 4 5" t)))

(ert-deftest ohf-test-skip-keep-no-weekdays ()
  "Habits without HABIT_WEEKDAYS are never skipped."
  (should-not (ohf-test--run-skip '(1 10 2024) nil)))

(ert-deftest ohf-test-skip-weekend-for-weekday-habit ()
  "Saturday is skipped for weekday-only habit."
  ;; Saturday 2024-01-06 (ISO 6)
  (should (ohf-test--run-skip '(1 6 2024) "1 2 3 4 5")))

(ert-deftest ohf-test-skip-keep-weekend-for-weekend-habit ()
  "Saturday is kept for weekend-only habit."
  ;; Saturday 2024-01-06 (ISO 6)
  (should-not (ohf-test--run-skip '(1 6 2024) "6 7")))

(ert-deftest ohf-test-skip-inactive-outside-get-scheduled ()
  "Skip function is a no-op when not inside get-scheduled context."
  ;; rendering-date is nil (not inside get-scheduled advice)
  (should-not (ohf-test--run-skip nil "1 2 4 5")))

;;; =========================================================
;;; Activation / deactivation
;;; =========================================================

(ert-deftest ohf-test-activate-deactivate ()
  "Activation adds advice, deactivation removes it."
  ;; Ensure clean state
  (when org-habit-flex--active (org-habit-flex-deactivate))
  (should-not org-habit-flex--active)

  (let ((org-agenda-skip-function-global nil))
    (org-habit-flex-activate)
    (should org-habit-flex--active)
    (should (advice-member-p #'org-habit-flex--parse-todo-advice 'org-habit-parse-todo))
    (should (advice-member-p #'org-habit-flex--build-graph-advice 'org-habit-build-graph))
    (should (advice-member-p #'org-habit-flex--get-faces-advice 'org-habit-get-faces))
    (should (memq #'org-habit-flex--adjust-schedule org-todo-repeat-hook))
    (should (advice-member-p #'org-habit-flex--get-scheduled-advice 'org-agenda-get-scheduled))
    (should (eq #'org-habit-flex-agenda-skip org-agenda-skip-function-global))

    (org-habit-flex-deactivate)
    (should-not org-habit-flex--active)
    (should-not (advice-member-p #'org-habit-flex--parse-todo-advice 'org-habit-parse-todo))
    (should-not (advice-member-p #'org-habit-flex--build-graph-advice 'org-habit-build-graph))
    (should-not (advice-member-p #'org-habit-flex--get-faces-advice 'org-habit-get-faces))
    (should-not (memq #'org-habit-flex--adjust-schedule org-todo-repeat-hook))
    (should-not (advice-member-p #'org-habit-flex--get-scheduled-advice 'org-agenda-get-scheduled))
    (should-not org-agenda-skip-function-global)))

(ert-deftest ohf-test-double-activate-is-safe ()
  "Activating twice doesn't double-add advice."
  (when org-habit-flex--active (org-habit-flex-deactivate))
  (org-habit-flex-activate)
  (org-habit-flex-activate)
  (should org-habit-flex--active)
  ;; Clean up
  (org-habit-flex-deactivate))

;;; org-habit-flex-test.el ends here
