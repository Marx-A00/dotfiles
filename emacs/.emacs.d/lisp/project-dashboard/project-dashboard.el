;;; project-dashboard.el --- Project dashboard for Emacs -*- lexical-binding: t; -*-

;; Author: Marcos Andrade
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (projectile "2.0"))
;; Keywords: project, dashboard, tasks

;;; Commentary:

;; A project dashboard that opens when switching projects via Projectile,
;; showing Task Master tasks and TODO items with quick-action keybindings.

;;; Code:

(require 'json)
(require 'projectile)
(require 'project-dashboard-art)

;;; Customization

(defgroup project-dashboard nil
  "Project dashboard settings."
  :group 'projectile
  :prefix "project-dashboard-")

(defcustom project-dashboard-show-taskmaster t
  "Whether to show Task Master tasks in the dashboard."
  :type 'boolean
  :group 'project-dashboard)

(defcustom project-dashboard-show-todo-files t
  "Whether to show TODO items from project files."
  :type 'boolean
  :group 'project-dashboard)

(defcustom project-dashboard-max-tasks 10
  "Maximum number of tasks to display per section."
  :type 'integer
  :group 'project-dashboard)

(defcustom project-dashboard-todo-files '("TODO.org" "TODO.md")
  "List of TODO file names to search for in project root."
  :type '(repeat string)
  :group 'project-dashboard)

(defcustom project-dashboard-auto-refresh t
  "Whether to automatically refresh the dashboard."
  :type 'boolean
  :group 'project-dashboard)

(defcustom project-dashboard-project-styles
  '(("rec" . (:title-color "#fb4934" :art-color "#F1BE49" :separator-color "#FFFBEB")))
  "Alist mapping project names to style overrides.
Each entry is (PROJECT-NAME . PLIST) where PLIST can contain:
  :title-color    - color for the project title
  :art-color      - color for the ASCII art
  :art-index      - specific ASCII art index (0-based)
  :separator-char - character for separator lines
  :separator-color - color for separator lines"
  :type '(alist :key-type string
                :value-type (plist :options ((:title-color string)
                                             (:art-color string)
                                             (:art-index integer)
                                             (:separator-char character))))
  :group 'project-dashboard)

(defcustom project-dashboard-refresh-interval 5
  "Seconds between auto-refresh when `project-dashboard-auto-refresh' is enabled."
  :type 'integer
  :group 'project-dashboard)

;;; Faces (Gruvbox-compatible)

(defface project-dashboard-header-face
  '((t :foreground "#fabd2f" :weight bold :height 1.4))
  "Face for the project name header."
  :group 'project-dashboard)

(defface project-dashboard-section-face
  '((t :foreground "#83a598" :weight bold :height 1.1))
  "Face for section headers."
  :group 'project-dashboard)

(defface project-dashboard-task-title-face
  '((t :foreground "#ebdbb2"))
  "Face for task titles."
  :group 'project-dashboard)

(defface project-dashboard-status-pending-face
  '((t :foreground "#928374"))
  "Face for pending status."
  :group 'project-dashboard)

(defface project-dashboard-status-in-progress-face
  '((t :foreground "#fabd2f"))
  "Face for in-progress status."
  :group 'project-dashboard)

(defface project-dashboard-status-done-face
  '((t :foreground "#b8bb26"))
  "Face for done status."
  :group 'project-dashboard)

(defface project-dashboard-priority-high-face
  '((t :foreground "#fb4934"))
  "Face for high priority indicator."
  :group 'project-dashboard)

(defface project-dashboard-key-face
  '((t :foreground "#fabd2f" :weight bold))
  "Face for keybinding hints."
  :group 'project-dashboard)

(defface project-dashboard-separator-face
  '((t :foreground "#665c54"))
  "Face for separator lines."
  :group 'project-dashboard)

;;; Buffer-local Variables

(defvar-local project-dashboard--project-root nil
  "The project root directory for this dashboard buffer.")

(defvar-local project-dashboard--taskmaster-data nil
  "Cached Task Master data for this dashboard.")

(defvar-local project-dashboard--todo-data nil
  "Cached TODO file data for this dashboard.")

(defvar-local project-dashboard--refresh-timer nil
  "Timer for auto-refreshing this dashboard buffer.")

;;; Data Layer - Task Master

(defun project-dashboard--get-active-tag (project-root)
  "Get the active Task Master tag for PROJECT-ROOT.
Returns the tag name from state.json, or \"master\" as fallback."
  (let ((state-file (expand-file-name ".taskmaster/state.json" project-root)))
    (if (file-exists-p state-file)
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-key-type 'symbol)
                   (state (json-read-file state-file)))
              (or (alist-get 'currentTag state) "master"))
          (error "master"))
      "master")))

(defun project-dashboard--read-taskmaster-json (project-root)
  "Read and parse Task Master tasks.json from PROJECT-ROOT.
Returns nil if file doesn't exist or is invalid."
  (let ((tasks-file (expand-file-name ".taskmaster/tasks/tasks.json" project-root)))
    (when (file-exists-p tasks-file)
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (data (json-read-file tasks-file))
                 (active-tag (project-dashboard--get-active-tag project-root))
                 (tag-symbol (intern active-tag)))
            ;; Task Master stores tasks under tag name
            ;; Structure is: { "tagname": { "tasks": [...] } }
            (or
             ;; Try active tag
             (alist-get 'tasks (alist-get tag-symbol data))
             ;; Fallback to master
             (alist-get 'tasks (alist-get 'master data))
             ;; Try legacy format: { "tasks": [...] }
             (alist-get 'tasks data)))
        (error
         (message "project-dashboard: Error reading tasks.json: %s" err)
         nil)))))

(defun project-dashboard--parse-tasks (tasks &optional filter-statuses)
  "Parse TASKS and optionally filter by FILTER-STATUSES.
FILTER-STATUSES is a list of status strings like (\"pending\" \"in-progress\").
Returns a list of task plists with :id, :title, :status, :priority, :dependencies."
  (let ((filtered-tasks
         (if filter-statuses
             (seq-filter (lambda (task)
                           (member (alist-get 'status task) filter-statuses))
                         tasks)
           tasks)))
    (mapcar (lambda (task)
              (list :id (alist-get 'id task)
                    :title (alist-get 'title task)
                    :status (alist-get 'status task)
                    :priority (alist-get 'priority task)
                    :dependencies (alist-get 'dependencies task)
                    :description (alist-get 'description task)))
            filtered-tasks)))

(defun project-dashboard--find-next-task (all-tasks)
  "Find the next task to work on from ALL-TASKS (raw alist format).
Returns a plist for the first in-progress task, or first pending task
whose dependencies are all done, or nil."
  (let* ((done-ids (mapcar (lambda (t)
                             (format "%s" (alist-get 'id t)))
                           (seq-filter (lambda (t)
                                         (string= (alist-get 'status t) "done"))
                                       all-tasks)))
         ;; First check for in-progress tasks
         (in-progress (seq-find (lambda (t)
                                  (string= (alist-get 'status t) "in-progress"))
                                all-tasks))
         ;; Then find first pending task with all deps satisfied
         (next-pending (seq-find
                        (lambda (t)
                          (and (string= (alist-get 'status t) "pending")
                               (let ((deps (alist-get 'dependencies t)))
                                 (or (null deps)
                                     (seq-every-p
                                      (lambda (dep)
                                        (member (format "%s" dep) done-ids))
                                      deps)))))
                        all-tasks))
         (result (or in-progress next-pending)))
    (when result
      (list :id (alist-get 'id result)
            :title (alist-get 'title result)
            :status (alist-get 'status result)
            :priority (alist-get 'priority result)
            :description (alist-get 'description result)))))

(defun project-dashboard--get-recently-completed (tasks &optional limit)
  "Get recently completed tasks from TASKS, up to LIMIT (default 5).
Returns list of completed task alists ordered by ID descending (higher = more recent)."
  (when tasks
    (let* ((done-tasks (seq-filter
                        (lambda (task)
                          (string= (alist-get 'status task) "done"))
                        tasks))
           (sorted-done (seq-sort
                         (lambda (a b)
                           (> (string-to-number (format "%s" (alist-get 'id a)))
                              (string-to-number (format "%s" (alist-get 'id b)))))
                         done-tasks)))
      (seq-take sorted-done (or limit 5)))))

;;; Data Layer - TODO Files

(defun project-dashboard--read-todo-org (file-path)
  "Read TODO items from an Org file at FILE-PATH.
Returns a list of plists with :title, :state, :priority."
  (when (file-exists-p file-path)
    (require 'org)
    (with-temp-buffer
      (insert-file-contents file-path)
      (org-mode)
      (let (todos)
        (org-map-entries
         (lambda ()
           (let* ((heading (org-get-heading t t t t))
                  (todo-state (org-get-todo-state))
                  (priority (org-entry-get nil "PRIORITY")))
             (when todo-state
               (push (list :title heading
                           :state todo-state
                           :priority priority)
                     todos)))))
        (nreverse todos)))))

(defun project-dashboard--read-todo-md (file-path)
  "Read TODO items from a Markdown file at FILE-PATH.
Looks for lines starting with '- [ ]' or '- [x]'.
Returns a list of plists with :title, :state."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (let (todos)
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*- \\[\\([ xX]\\)\\]\\s-*\\(.+\\)$" nil t)
          (let ((checked (not (string= (match-string 1) " ")))
                (title (string-trim (match-string 2))))
            (push (list :title title
                        :state (if checked "DONE" "TODO")
                        :priority nil)
                  todos)))
        (nreverse todos)))))

(defun project-dashboard--find-todo-file (project-root)
  "Find the first existing TODO file in PROJECT-ROOT.
Returns (FILE-PATH . TYPE) where TYPE is `org' or `md', or nil."
  (catch 'found
    (dolist (filename project-dashboard-todo-files)
      (let ((file-path (expand-file-name filename project-root)))
        (when (file-exists-p file-path)
          (throw 'found
                 (cons file-path
                       (if (string-suffix-p ".org" filename) 'org 'md))))))))

;;; Rendering Layer

(defvar-local project-dashboard--current-art nil
  "The current ASCII art displayed in this dashboard buffer.")

(defun project-dashboard--get-project-style (project-name)
  "Get the style plist for PROJECT-NAME, or nil if none defined."
  (cdr (assoc project-name project-dashboard-project-styles)))

(defun project-dashboard--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun project-dashboard--str-len (str)
  "Calculate STR length in character units from pixel width."
  (let ((width (frame-char-width))
        (len (project-dashboard--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))

(defun project-dashboard--center-text (start end)
  "Center the text between START and END using display properties."
  (let* ((max-width (project-dashboard--find-max-width start end))
         (prefix (propertize " " 'display
                             `(space . (:align-to (- center ,(/ (float max-width) 2)))))))
    (add-text-properties start end
                         `(line-prefix ,prefix wrap-prefix ,prefix))))

(defun project-dashboard--find-max-width (start end)
  "Find the maximum line width between START and END."
  (let ((max-width 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((line-str (buffer-substring (line-beginning-position) (line-end-position)))
               (line-width (project-dashboard--str-len line-str)))
          (when (> line-width max-width)
            (setq max-width line-width)))
        (forward-line 1)))
    max-width))

(defun project-dashboard--insert-centered (&rest strings)
  "Insert STRINGS centered in the buffer."
  (let ((start (point)))
    (apply #'insert strings)
    (project-dashboard--center-text start (point))))

(defun project-dashboard--render-header (project-name)
  "Render the dashboard header with PROJECT-NAME."
  (let* ((style (project-dashboard--get-project-style project-name))
         (art-color (or (plist-get style :art-color) "#fabd2f"))
         (title-color (plist-get style :title-color))
         (separator-char (or (plist-get style :separator-char) ?─))
         (separator-color (plist-get style :separator-color))
         (art-index (plist-get style :art-index)))
    (insert "\n")
    ;; Render ASCII art (use cached art, specific index, or pick random)
    (unless project-dashboard--current-art
      (setq project-dashboard--current-art
            (if art-index
                (project-dashboard-art-by-index art-index)
              (project-dashboard-art-random))))
    (let ((art-start (point)))
      (dolist (line project-dashboard--current-art)
        (insert (propertize (format "%s\n" line)
                            'face `(:foreground ,art-color))))
      (project-dashboard--center-text art-start (point)))
    (insert "\n")
    ;; Project name (centered) - use custom color or default face
    (project-dashboard--insert-centered
     (propertize (format "%s\n" project-name)
                 'face (if title-color
                           `(:foreground ,title-color :weight bold :height 1.4)
                         'project-dashboard-header-face)))
    ;; Separator (centered)
    (let ((separator (make-string (min 60 (+ 2 (length project-name))) separator-char)))
      (project-dashboard--insert-centered
       (propertize (format "%s\n" separator)
                   'face (if separator-color
                             `(:foreground ,separator-color)
                           'project-dashboard-separator-face))))
    (insert "\n")))

(defun project-dashboard--render-status (status)
  "Render STATUS with appropriate face."
  (let ((face (pcase status
                ("in-progress" 'project-dashboard-status-in-progress-face)
                ("pending" 'project-dashboard-status-pending-face)
                ("done" 'project-dashboard-status-done-face)
                (_ 'project-dashboard-status-pending-face))))
    (propertize (format "%-12s" status) 'face face)))

(defun project-dashboard--render-priority (priority)
  "Render PRIORITY indicator if high."
  (if (and priority (string= priority "high"))
      (propertize "!" 'face 'project-dashboard-priority-high-face)
    " "))

(defvar-local project-dashboard--active-tag nil
  "The active Task Master tag for this dashboard.")

(defun project-dashboard--find-next-task (tasks)
  "Find the next task to work on from TASKS (raw alist format).
Returns a plist with :id, :title, :is-in-progress, or nil.
Priority: in-progress task first, then first pending with deps satisfied."
  (when tasks
    (let* ((done-ids (mapcar (lambda (t)
                               (format "%s" (alist-get 'id t)))
                             (seq-filter (lambda (t)
                                           (string= (alist-get 'status t) "done"))
                                         tasks)))
           ;; First check for in-progress task
           (in-progress (seq-find (lambda (t)
                                    (string= (alist-get 'status t) "in-progress"))
                                  tasks))
           ;; Then find first pending task with all deps satisfied
           (next-pending (and (not in-progress)
                              (seq-find
                               (lambda (t)
                                 (and (string= (alist-get 'status t) "pending")
                                      (let ((deps (alist-get 'dependencies t)))
                                        (or (null deps)
                                            (seq-every-p
                                             (lambda (dep)
                                               (member (format "%s" dep) done-ids))
                                             deps)))))
                               tasks)))
           (result (or in-progress next-pending)))
      (when result
        (list :id (alist-get 'id result)
              :title (alist-get 'title result)
              :is-in-progress (string= (alist-get 'status result) "in-progress"))))))

(defun project-dashboard--render-next-task (next-task)
  "Render the Next Task or In Progress section for NEXT-TASK plist."
  (let ((header (if (plist-get next-task :is-in-progress)
                    "In Progress"
                  "Next Task")))
    (insert (propertize (format "  %s" header) 'face 'project-dashboard-section-face))
    (when project-dashboard--active-tag
      (insert (propertize (format " (%s)" project-dashboard--active-tag)
                          'face 'project-dashboard-separator-face)))
    (insert "\n\n")
    (if next-task
        (let* ((id (plist-get next-task :id))
               (title (plist-get next-task :title)))
          (insert "    ")
          (insert (propertize (format "#%-4s" id) 'face 'project-dashboard-separator-face))
          (insert (propertize (truncate-string-to-width title 70 nil nil "...")
                              'face 'project-dashboard-task-title-face))
          (insert "\n"))
      (insert (propertize "    No next task\n" 'face 'project-dashboard-status-pending-face)))
    (insert "\n")))

(defun project-dashboard--render-tasks-section (tasks)
  "Render the Task Master TASKS section."
  (insert (propertize "  Tasks" 'face 'project-dashboard-section-face))
  (when project-dashboard--active-tag
    (insert (propertize (format " (%s)" project-dashboard--active-tag)
                        'face 'project-dashboard-separator-face)))
  (insert "\n\n")
  (if (null tasks)
      (insert (propertize "    No tasks found\n" 'face 'project-dashboard-status-pending-face))
    (let ((count 0))
      (dolist (task tasks)
        (when (< count project-dashboard-max-tasks)
          (let* ((id (plist-get task :id))
                 (title (plist-get task :title))
                 (status (plist-get task :status))
                 (priority (plist-get task :priority)))
            (insert "    ")
            (insert (propertize (format "#%-4s" id) 'face 'project-dashboard-separator-face))
            (insert (project-dashboard--render-status status))
            (insert (project-dashboard--render-priority priority))
            (insert " ")
            (insert (propertize (truncate-string-to-width (or title "") 70 nil nil "...")
                                'face 'project-dashboard-task-title-face))
            (insert "\n"))
          (cl-incf count)))))
  (insert "\n"))

(defun project-dashboard--render-todo-section (todos todo-file-path)
  "Render the TODO section with TODOS from TODO-FILE-PATH."
  (insert (propertize "  TODOs" 'face 'project-dashboard-section-face))
  (insert "  ")
  (insert (propertize (format "(%s)" (file-name-nondirectory todo-file-path))
                      'face 'project-dashboard-separator-face))
  (insert "\n\n")
  (if (null todos)
      (insert (propertize "    No TODO items found\n" 'face 'project-dashboard-status-pending-face))
    (let ((count 0))
      (dolist (todo todos)
        (when (< count project-dashboard-max-tasks)
          (let* ((title (plist-get todo :title))
                 (state (plist-get todo :state))
                 (priority (plist-get todo :priority)))
            (insert "    ")
            (insert (propertize (format "[%-4s]" state)
                                'face (if (member state '("DONE" "done"))
                                          'project-dashboard-status-done-face
                                        'project-dashboard-status-pending-face)))
            (when (and priority (string= priority "A"))
              (insert (propertize " !" 'face 'project-dashboard-priority-high-face)))
            (insert "  ")
            (insert (propertize (truncate-string-to-width (or title "") 50 nil nil "...")
                                'face 'project-dashboard-task-title-face))
            (insert "\n"))
          (cl-incf count)))))
  (insert "\n"))

(defun project-dashboard--render-actions-legend ()
  "Render the quick actions legend horizontally at the bottom."
  (project-dashboard--insert-centered
   (propertize (format "%s\n" (make-string 60 ?─)) 'face 'project-dashboard-separator-face))
  (insert "\n")
  ;; Build horizontal legend string
  (let* ((actions '(("a" . "Agent") ("d" . "Dired") ("m" . "Magit") ("f" . "Find")
                    ("t" . "Tasks") ("r" . "Refresh") ("q" . "Quit")))
         (legend-parts
          (mapcar (lambda (action)
                    (concat (propertize (format "[%s]" (car action)) 
                                        'face 'project-dashboard-key-face)
                            (cdr action)))
                  actions))
         (legend-str (string-join legend-parts "  ")))
    (project-dashboard--insert-centered (format "%s\n" legend-str))))

(defun project-dashboard--render ()
  "Render the complete dashboard for the current project."
  (let* ((inhibit-read-only t)
         (project-root project-dashboard--project-root)
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (has-taskmaster nil)
         (has-todo nil))
    (erase-buffer)
    
    ;; Header (ASCII art + project name)
    (project-dashboard--render-header project-name)
    
    ;; Actions legend (right below title)
    (project-dashboard--render-actions-legend)
    
    (insert "\n")
    
    ;; Task Master section
    (when project-dashboard-show-taskmaster
      (let* ((active-tag (project-dashboard--get-active-tag project-root))
             (tasks (project-dashboard--read-taskmaster-json project-root))
             (next-task (project-dashboard--find-next-task tasks))
             (filtered (project-dashboard--parse-tasks tasks '("in-progress" "pending"))))
        (setq project-dashboard--active-tag active-tag)
        (setq project-dashboard--taskmaster-data filtered)
        (when tasks
          (setq has-taskmaster t)
          ;; Next task / In Progress section
          (project-dashboard--render-next-task next-task)
          (project-dashboard--render-tasks-section filtered))))
    
    ;; TODO file section
    (when project-dashboard-show-todo-files
      (let ((todo-info (project-dashboard--find-todo-file project-root)))
        (when todo-info
          (let* ((file-path (car todo-info))
                 (file-type (cdr todo-info))
                 (todos (if (eq file-type 'org)
                            (project-dashboard--read-todo-org file-path)
                          (project-dashboard--read-todo-md file-path))))
            (setq project-dashboard--todo-data (cons file-path todos))
            (setq has-todo t)
            (project-dashboard--render-todo-section todos file-path)))))
    
    ;; Show message if no tasks found
    (when (and (not has-taskmaster) (not has-todo))
      (insert (propertize "  No tasks or TODO files found in this project\n\n"
                          'face 'project-dashboard-status-pending-face)))
    
    (goto-char (point-min))))

;;; Action Functions

(defun project-dashboard-open-agent-shell ()
  "Open agent-shell for the current project."
  (interactive)
  (let ((default-directory project-dashboard--project-root))
    (if (fboundp 'agent-shell)
        (agent-shell)
      (message "agent-shell not available"))))

(defun project-dashboard-open-dired ()
  "Open dired at project root."
  (interactive)
  (dired project-dashboard--project-root))

(defun project-dashboard-open-magit ()
  "Open magit for the current project."
  (interactive)
  (let ((default-directory project-dashboard--project-root))
    (if (fboundp 'magit-status)
        (magit-status)
      (message "magit not available"))))

(defun project-dashboard-find-file ()
  "Find file in the current project."
  (interactive)
  (let ((default-directory project-dashboard--project-root))
    (cond
     ((fboundp 'counsel-projectile-find-file)
      (counsel-projectile-find-file))
     ((fboundp 'projectile-find-file)
      (projectile-find-file))
     (t
      (call-interactively #'find-file)))))

(defun project-dashboard-open-tasks-file ()
  "Open the tasks file for editing."
  (interactive)
  (let ((taskmaster-file (expand-file-name ".taskmaster/tasks/tasks.json"
                                           project-dashboard--project-root))
        (todo-file (car project-dashboard--todo-data)))
    (cond
     ;; Both exist - use ivy to choose if available
     ((and (file-exists-p taskmaster-file) todo-file)
      (if (fboundp 'ivy-read)
          (ivy-read "Open tasks file: "
                    (list (cons "Task Master (tasks.json)" taskmaster-file)
                          (cons (format "TODO (%s)" (file-name-nondirectory todo-file)) todo-file))
                    :action (lambda (choice)
                              (find-file (cdr choice))))
        ;; Fallback to Task Master if no ivy
        (find-file taskmaster-file)))
     ((file-exists-p taskmaster-file)
      (find-file taskmaster-file))
     (todo-file
      (find-file todo-file))
     (t
      (message "No tasks file found in project")))))

(defun project-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (project-dashboard--render)
  (message "Dashboard refreshed"))

(defun project-dashboard-new-art ()
  "Pick a new random ASCII art and refresh."
  (interactive)
  (setq project-dashboard--current-art (project-dashboard-art-random))
  (project-dashboard--render)
  (message "New art!"))

(defun project-dashboard--auto-refresh ()
  "Auto-refresh the dashboard if buffer is visible."
  (when (and (buffer-live-p (current-buffer))
             (get-buffer-window (current-buffer)))
    (let ((inhibit-message t))  ; Suppress "Dashboard refreshed" message
      (project-dashboard--render))))

(defun project-dashboard--start-auto-refresh ()
  "Start the auto-refresh timer for current dashboard buffer."
  (when (and project-dashboard-auto-refresh
             (not project-dashboard--refresh-timer))
    (setq project-dashboard--refresh-timer
          (run-with-timer project-dashboard-refresh-interval
                          project-dashboard-refresh-interval
                          (let ((buf (current-buffer)))
                            (lambda ()
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (project-dashboard--auto-refresh)))))))))

(defun project-dashboard--stop-auto-refresh ()
  "Stop the auto-refresh timer for current dashboard buffer."
  (when project-dashboard--refresh-timer
    (cancel-timer project-dashboard--refresh-timer)
    (setq project-dashboard--refresh-timer nil)))

(defun project-dashboard-quit ()
  "Quit the dashboard and return to previous buffer."
  (interactive)
  (project-dashboard--stop-auto-refresh)
  (quit-window))

;;; Major Mode

(defvar project-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'project-dashboard-open-agent-shell)
    (define-key map (kbd "d") #'project-dashboard-open-dired)
    (define-key map (kbd "m") #'project-dashboard-open-magit)
    (define-key map (kbd "f") #'project-dashboard-find-file)
    (define-key map (kbd "t") #'project-dashboard-open-tasks-file)
    (define-key map (kbd "r") #'project-dashboard-refresh)
    (define-key map (kbd "g") #'project-dashboard-refresh)
    (define-key map (kbd "q") #'project-dashboard-quit)
    (define-key map (kbd "RET") #'project-dashboard-find-file)
    map)
  "Keymap for `project-dashboard-mode'.")

(define-derived-mode project-dashboard-mode special-mode "ProjDash"
  "Major mode for displaying project dashboard.

\\{project-dashboard-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local cursor-type nil)
  (setq-local show-trailing-whitespace nil))

;; Evil mode integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'project-dashboard-mode 'normal)
  (evil-define-key 'normal project-dashboard-mode-map
    (kbd "a") #'project-dashboard-open-agent-shell
    (kbd "d") #'project-dashboard-open-dired
    (kbd "m") #'project-dashboard-open-magit
    (kbd "f") #'project-dashboard-find-file
    (kbd "t") #'project-dashboard-open-tasks-file
    (kbd "r") #'project-dashboard-refresh
    (kbd "R") #'project-dashboard-new-art
    (kbd "gr") #'project-dashboard-refresh
    (kbd "q") #'project-dashboard-quit
    (kbd "RET") #'project-dashboard-find-file))

;;; Entry Points

;;;###autoload
(defun project-dashboard-open (&optional project-root)
  "Open the project dashboard for PROJECT-ROOT.
If PROJECT-ROOT is nil, use current projectile project."
  (interactive)
  (let* ((root (or project-root
                   (projectile-project-root)
                   default-directory))
         (buf-name (format "*Project: %s*"
                           (file-name-nondirectory (directory-file-name root))))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'project-dashboard-mode)
        (project-dashboard-mode))
      (setq project-dashboard--project-root root)
      (project-dashboard--render)
      (project-dashboard--start-auto-refresh))
    (switch-to-buffer buf)))

;;;###autoload
(defun project-dashboard--projectile-switch-action ()
  "Action to run when switching projects via projectile.
Opens the project dashboard for the selected project."
  (project-dashboard-open (projectile-project-root)))

;;; Project Launcher

(defcustom project-dashboard-projects
  '(("dotfiles" . "~/.dotfiles")
    ("sandbox" . "~/.emacs-sandbox")
    ;; Roaming
    ("roaming/claude" . "~/roaming/claude")
    ("roaming/code" . "~/roaming/code")
    ("roaming/notes" . "~/roaming/notes")
    ("roaming/personal" . "~/roaming/personal")
    ;; Work
    ("work/cloudburst" . "~/work/cloudburst")
    ("work/experian" . "~/work/experian")
    ("work/omi-live" . "~/work/omi-live")
    ("work/virtual-bid" . "~/work/virtual-bid")
    ("work/york" . "~/work/york"))
  "Alist of project names and their paths for the project launcher."
  :type '(alist :key-type string :value-type string)
  :group 'project-dashboard)

;;;###autoload
(defun project-dashboard-launch ()
  "Launch a project dashboard from a list of known projects."
  (interactive)
  (if (fboundp 'ivy-read)
      (ivy-read "Project: " project-dashboard-projects
                :action (lambda (choice)
                          (project-dashboard-open (expand-file-name (cdr choice)))))
    ;; Fallback to completing-read
    (let* ((choice (completing-read "Project: " project-dashboard-projects))
           (path (cdr (assoc choice project-dashboard-projects))))
      (project-dashboard-open (expand-file-name path)))))

;;;###autoload
(defun project-dashboard-add-project (path name)
  "Add a project at PATH with NAME to the project list."
  (interactive
   (list (read-directory-name "Project path: " "~/" nil t)
         (read-string "Project name: ")))
  (let ((entry (cons name (abbreviate-file-name path))))
    (add-to-list 'project-dashboard-projects entry t)
    (customize-save-variable 'project-dashboard-projects project-dashboard-projects)
    (message "Added project: %s -> %s" name path)))

(provide 'project-dashboard)

;;; project-dashboard.el ends here
