;;; agent-shell-manager-custom.el --- Custom extensions for agent-shell-manager -*- lexical-binding: t; -*-

;; Extends agent-shell-manager with:
;; - Preview mode (p to enter, q to exit, j/k live preview)
;; - Project grouping (TODO)
;; - Status filtering (TODO)

;;; Code:

;; ============================================================
;; Agent Shell Manager Customization (mr-x/asm-*)
;; Features: Preview Mode, Project Grouping, Status Filtering
;; ============================================================

;; === Preview Mode State ===
(defvar mr-x/asm-preview-active nil
  "Non-nil when preview mode is active in agent-shell-manager.")

(defvar mr-x/asm-preview-saved-config nil
  "Window configuration saved before entering preview mode.")

(defvar mr-x/asm-preview-buffer nil
  "Buffer currently being previewed. Used to find preview window
dynamically — we NEVER store window objects.")

(defvar mr-x/asm-preview-highlight-overlay nil
  "Overlay used to highlight the current row during preview mode.")

(defface mr-x/asm-preview-highlight
  '((t :background "#3c3836" :extend t))  ;; gruvbox bg1 — subtle highlight
  "Face for the currently previewed entry in the manager.")

;; === Preview Mode Window Utilities ===
(defun mr-x/asm-preview-window ()
  "Find the preview window dynamically. Returns nil if not found.
Walks all windows in the current frame and returns the one that is
NOT displaying the manager buffer. Never stores a window reference."
  (when mr-x/asm-preview-active
    (let ((mgr-buf (get-buffer "*Agent-Shell Buffers*"))
          (result nil))
      ;; First try: find the window showing our preview buffer
      (when mr-x/asm-preview-buffer
        (walk-windows
         (lambda (w)
           (when (and (not result)
                      (not (eq (window-buffer w) mgr-buf))
                      (eq (window-buffer w) mr-x/asm-preview-buffer))
             (setq result w)))
         nil (selected-frame)))
      ;; Fallback: find any non-manager window
      (unless result
        (walk-windows
         (lambda (w)
           (when (and (not result)
                      (not (eq (window-buffer w) mgr-buf)))
             (setq result w)))
         nil (selected-frame)))
      result)))

;; === Preview Mode: Update ===
(defun mr-x/asm-preview-update ()
  "Update the preview pane to show the buffer under cursor.
Called via `post-command-hook' in the manager buffer."
  (when (and mr-x/asm-preview-active
             (derived-mode-p 'agent-shell-manager-mode))
    ;; Update highlight overlay on current line
    (if (and mr-x/asm-preview-highlight-overlay
             (overlay-buffer mr-x/asm-preview-highlight-overlay))
        (move-overlay mr-x/asm-preview-highlight-overlay
                      (line-beginning-position) (1+ (line-end-position)))
      (setq mr-x/asm-preview-highlight-overlay
            (make-overlay (line-beginning-position) (1+ (line-end-position))))
      (overlay-put mr-x/asm-preview-highlight-overlay 'face 'mr-x/asm-preview-highlight)
      (overlay-put mr-x/asm-preview-highlight-overlay 'priority 100))
    ;; Update preview pane
    (when-let* ((buffer (tabulated-list-get-id)))
      (when (buffer-live-p buffer)
        (let ((preview-win (mr-x/asm-preview-window))
              (switched (not (eq buffer mr-x/asm-preview-buffer))))
          (when (and preview-win (window-live-p preview-win))
            (setq mr-x/asm-preview-buffer buffer)
            (set-window-buffer preview-win buffer)
            ;; Only scroll to bottom when switching to a different buffer
            (when switched
              (with-selected-window preview-win
                (goto-char (point-max))
                (recenter -3)))))))))

;; === Preview Mode: Enter / Exit ===
(defun mr-x/asm-enter-preview ()
  "Enter preview mode. Saves window config, creates split layout.
Top pane = preview of shell buffer, bottom pane = manager (15 lines)."
  (interactive)
  (when mr-x/asm-preview-active
    (message "Already in preview mode")
    (cl-return-from mr-x/asm-enter-preview))
  ;; Save current window configuration for restoration
  (setq mr-x/asm-preview-saved-config (current-window-configuration))
  ;; Grab the existing manager buffer — don't create a new one
  (let ((mgr-buf (or agent-shell-manager--global-buffer
                     (get-buffer "*Agent-Shell Buffers*"))))
    (unless mgr-buf
      (user-error "No manager buffer. Open it first with agent-shell-manager-toggle"))
    ;; Close the manager's existing window first (side windows survive delete-other-windows)
    (let ((mgr-win (get-buffer-window mgr-buf)))
      (when (and mgr-win (window-live-p mgr-win))
        ;; If we're IN the manager window, move out first
        (when (eq (selected-window) mgr-win)
          (select-window (window-main-window)))
        (delete-window mgr-win)))
    ;; Clean slate — now safe since side window is gone
    (delete-other-windows)
    ;; Current window becomes the preview pane (top, large)
    ;; Show first agent-shell buffer as initial preview
    (let ((first-shell (car (agent-shell-buffers))))
      (when first-shell
        (setq mr-x/asm-preview-buffer first-shell)
        (set-window-buffer (selected-window) first-shell)))
    ;; Split: bottom window for the manager
    (let ((manager-win (split-window-below -15)))
      (select-window manager-win)
      (switch-to-buffer mgr-buf)
      ;; Refresh the existing manager (don't re-init the mode)
      (agent-shell-manager-refresh)
      ;; Activate preview
      (setq mr-x/asm-preview-active t)
      (add-hook 'post-command-hook #'mr-x/asm-preview-update nil t)
      ;; Trigger initial preview for whatever line cursor is on
      (mr-x/asm-preview-update)
      (message "Preview mode: j/k navigate, RET select, q quit"))))

(defun mr-x/asm-exit-preview ()
  "Exit preview mode and restore original window configuration."
  (setq mr-x/asm-preview-active nil)
  (setq mr-x/asm-preview-buffer nil)
  (remove-hook 'post-command-hook #'mr-x/asm-preview-update t)
  ;; Clean up highlight overlay
  (when (and mr-x/asm-preview-highlight-overlay
             (overlay-buffer mr-x/asm-preview-highlight-overlay))
    (delete-overlay mr-x/asm-preview-highlight-overlay))
  (setq mr-x/asm-preview-highlight-overlay nil)
  (when mr-x/asm-preview-saved-config
    (set-window-configuration mr-x/asm-preview-saved-config)
    (setq mr-x/asm-preview-saved-config nil)))

;; === Preview Mode: Context-Aware Navigation ===
(defun mr-x/asm-select ()
  "Select the buffer at point. If in preview mode, exit it first.
Keeps the manager open (matches default goto behavior)."
  (interactive)
  (if mr-x/asm-preview-active
      (let ((buffer (tabulated-list-get-id)))
        (mr-x/asm-exit-preview)
        (when (and buffer (buffer-live-p buffer))
          (agent-shell-manager-goto)))
    (agent-shell-manager-goto)))

(defun mr-x/asm-quit ()
  "Context-aware quit.
If preview mode is active, exit preview and restore windows.
Otherwise, close the manager window normally."
  (interactive)
  (if mr-x/asm-preview-active
      (mr-x/asm-exit-preview)
    (quit-window)))

;; ============================================================
;; === Status Filtering State ===
;; ============================================================

(defvar mr-x/asm-filter-status nil
  "Current status filter. nil = show all.")

(defvar mr-x/asm-filter-cycle '(nil "Working" "Waiting" "Ready" "Killed")
  "Cycle of filter values. nil means show all.")

;; ============================================================
;; === Project Grouping ===
;; ============================================================

(defun mr-x/asm-buffer-project-root (buffer)
  "Get the project root for BUFFER. Uses projectile if available,
falls back to `default-directory'. Returns abbreviated path."
  (with-current-buffer buffer
    (abbreviate-file-name
     (or (and (fboundp 'projectile-project-root)
              (ignore-errors (projectile-project-root)))
         default-directory))))

(defun mr-x/asm-entries-filtered-and-grouped (orig-fn)
  "Advice around `agent-shell-manager--entries'.
1. Calls original to get raw entries.
2. Filters by status (if `mr-x/asm-filter-status' is non-nil).
3. Sorts by project root (alphabetical), killed-to-bottom within each group."
  (let* ((entries (funcall orig-fn))
         ;; Step 1: Filter by status
         (entries (if mr-x/asm-filter-status
                      (seq-filter
                       (lambda (entry)
                         (let ((status (substring-no-properties (aref (cadr entry) 1))))
                           (string-equal-ignore-case status mr-x/asm-filter-status)))
                       entries)
                    entries))
         ;; Step 2: Annotate with project root
         (annotated (mapcar
                     (lambda (entry)
                       (let* ((buffer (car entry))
                              (project (if (buffer-live-p buffer)
                                           (mr-x/asm-buffer-project-root buffer)
                                         "~/")))
                         (cons project entry)))
                     entries)))
    ;; Step 3: Sort by project (alpha), killed-to-bottom within group
    (setq annotated
          (sort annotated
                (lambda (a b)
                  (let ((proj-a (car a))
                        (proj-b (car b))
                        (status-a (substring-no-properties (aref (caddr a) 1)))
                        (status-b (substring-no-properties (aref (caddr b) 1))))
                    (cond
                     ((not (string= proj-a proj-b))
                      (string< proj-a proj-b))
                     ((and (string= status-a "Killed") (not (string= status-b "Killed")))
                      nil)
                     ((and (not (string= status-a "Killed")) (string= status-b "Killed"))
                      t)
                     (t nil))))))
    ;; Strip annotations
    (mapcar #'cdr annotated)))

(advice-add 'agent-shell-manager--entries :around #'mr-x/asm-entries-filtered-and-grouped)

(defun mr-x/asm-inject-group-headers ()
  "Insert visual group headers between project boundaries in the manager buffer.
Headers are text-only lines with a `mr-x/asm-group-header' text property
so they can be cleanly removed on re-refresh."
  (when (derived-mode-p 'agent-shell-manager-mode)
    (let ((inhibit-read-only t)
          (last-project nil))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((entry-buffer (tabulated-list-get-id))
                 (project (when (and entry-buffer (buffer-live-p entry-buffer))
                            (mr-x/asm-buffer-project-root entry-buffer))))
            (when (and project (not (equal project last-project)))
              ;; Insert a header line ABOVE this entry
              (let* ((header-text (concat "-- " project " "
                                          (make-string (max 1 (- 60 (length project) 4)) ?-)
                                          "\n"))
                     (propertized (propertize header-text
                                             'face 'font-lock-comment-face
                                             'mr-x/asm-group-header t)))
                (insert propertized))
              (setq last-project project)))
          (forward-line 1))))))

(defun mr-x/asm-remove-group-headers ()
  "Remove all group header lines (identified by text property) from the manager buffer."
  (when (derived-mode-p 'agent-shell-manager-mode)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (if (get-text-property (point) 'mr-x/asm-group-header)
              (delete-region (line-beginning-position) (1+ (line-end-position)))
            (forward-line 1)))))))

(defun mr-x/asm-refresh-with-groups (orig-fn)
  "Advice around `agent-shell-manager-refresh'.
Removes old header lines, calls original refresh, then injects new headers.
Preserves cursor position, scroll offset, and highlight overlay."
  ;; Save state: which entry + how many lines from window top to cursor
  (let* ((target-buf (tabulated-list-get-id))
         (win (get-buffer-window (current-buffer)))
         (line-offset (when win
                        (count-lines (window-start win) (point)))))
    (mr-x/asm-remove-group-headers)
    (funcall orig-fn)
    (mr-x/asm-inject-group-headers)
    ;; Restore cursor to the same buffer entry
    (when target-buf
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (eq (tabulated-list-get-id) target-buf)
              (setq found t)
            (forward-line 1)))))
    ;; Restore scroll so cursor stays at same visual row in the window
    (when (and win (window-live-p win) line-offset)
      (set-window-start win
                        (save-excursion
                          (forward-line (- line-offset))
                          (point))
                        t))
    ;; Re-apply highlight overlay if preview mode is active
    (when mr-x/asm-preview-active
      (if (and mr-x/asm-preview-highlight-overlay
               (overlay-buffer mr-x/asm-preview-highlight-overlay))
          (move-overlay mr-x/asm-preview-highlight-overlay
                        (line-beginning-position) (1+ (line-end-position)))
        (setq mr-x/asm-preview-highlight-overlay
              (make-overlay (line-beginning-position) (1+ (line-end-position))))
        (overlay-put mr-x/asm-preview-highlight-overlay 'face 'mr-x/asm-preview-highlight)
        (overlay-put mr-x/asm-preview-highlight-overlay 'priority 100)))))

(advice-add 'agent-shell-manager-refresh :around #'mr-x/asm-refresh-with-groups)

(defun mr-x/asm-skip-group-headers ()
  "Post-command-hook to skip cursor past group header lines.
Only activates for known navigation commands (j/k/C-n/C-p/G/gg)."
  (when (and (derived-mode-p 'agent-shell-manager-mode)
             (get-text-property (line-beginning-position) 'mr-x/asm-group-header))
    ;; Only skip for actual navigation commands
    (let ((direction (cond
                      ((memq this-command '(next-line evil-next-line evil-next-visual-line
                                            evil-goto-line)) 1)
                      ((memq this-command '(previous-line evil-previous-line evil-previous-visual-line
                                            evil-goto-first-line)) -1)
                      (t nil))))  ;; unknown command — don't interfere
      (when direction
        (forward-line direction)
        (while (and (not (eobp)) (not (bobp))
                    (get-text-property (line-beginning-position) 'mr-x/asm-group-header))
          (forward-line direction))
        ;; If we went past the end, back up to last real line
        (when (eobp) (forward-line -1))))))

(add-hook 'agent-shell-manager-mode-hook
          (lambda ()
            (add-hook 'post-command-hook #'mr-x/asm-skip-group-headers nil t)))

;; ============================================================
;; === Status Filtering UI ===
;; ============================================================

(defun mr-x/asm-update-header-line ()
  "Update the manager buffer's header-line to show the current filter.
When filter is nil, reset to default (tabulated-list column header)."
  (when (and agent-shell-manager--global-buffer
             (buffer-live-p agent-shell-manager--global-buffer))
    (with-current-buffer agent-shell-manager--global-buffer
      (setq header-line-format
            (if mr-x/asm-filter-status
                (format " Filter: %s  [f to cycle]" mr-x/asm-filter-status)
              nil))  ;; nil reverts to tabulated-list default header
      ;; Re-init the header so column headers come back when filter is cleared
      (unless mr-x/asm-filter-status
        (tabulated-list-init-header)))))

(defun mr-x/asm-cycle-filter ()
  "Cycle through status filters. Updates header-line and refreshes."
  (interactive)
  (let* ((current-pos (cl-position mr-x/asm-filter-status mr-x/asm-filter-cycle
                                   :test #'equal))
         (next-pos (mod (1+ (or current-pos -1)) (length mr-x/asm-filter-cycle))))
    (setq mr-x/asm-filter-status (nth next-pos mr-x/asm-filter-cycle))
    (mr-x/asm-update-header-line)
    (agent-shell-manager-refresh)
    (message "Filter: %s" (or mr-x/asm-filter-status "All"))))

;; ============================================================
;; === Test Scenario ===
;; ============================================================

(defun mr-x/spawn-multi-project-test ()
  "Spawn agent shells across different projects to test grouping/filtering.
Creates shells in 3 different project directories with identifying messages,
then opens the manager buffer so you can test preview, grouping, and filtering."
  (interactive)
  (let ((projects '(("~/.dotfiles/" . "dotfiles shell — try editing emacs.org")
                    ("~/roaming/claude/" . "claude shell — general purpose")
                    ("~/roaming/projects/blog/" . "blog shell — writing project"))))
    ;; Spawn 2 shells per project for a richer test
    (dolist (proj projects)
      (let ((default-directory (expand-file-name (car proj)))
            (msg (cdr proj)))
        ;; First shell: insert identifying message
        (agent-shell t)
        (goto-char (point-max))
        (insert msg)
        ;; Second shell: leave empty (tests mixed state)
        (agent-shell t)))
    ;; Open the manager so you can immediately test
    (agent-shell-manager-toggle)
    (message "Spawned 6 test shells across 3 projects. Press p for preview, f for filter.")))

(provide 'agent-shell-manager-custom)
;;; agent-shell-manager-custom.el ends here
