;;; agent-shell-refs.el --- Reference/quote context attachments for agent-shell -*- lexical-binding: t -*-

;;; Commentary:
;; Select text from Claude's response (or anywhere in the agent-shell buffer),
;; hit a keybinding to "attach" it as context.  Attached refs are silently
;; prepended to your next prompt on send, then cleared.
;;
;; Three visual feedback channels:
;;   1. Modeline segment — 📎N when refs are queued
;;   2. Pulse — region flashes on capture
;;   3. Headerline — truncated snippets bar above the buffer
;;
;; Keybindings (configured externally):
;;   - Capture:  visual-mode binding → agent-shell-refs-capture
;;   - Preview:  agent-shell-refs-preview (show all in popup)
;;   - Clear:    agent-shell-refs-clear
;;   - Remove:   agent-shell-refs-remove (pick one to drop)

;;; Code:

(require 'cl-lib)
(require 'pulse)

;;; --- Data ---

(defvar-local agent-shell-refs--list nil
  "List of reference strings attached to this agent-shell buffer.
Each entry is a string captured from the buffer.")

;;; --- Faces ---

(defface agent-shell-refs-modeline-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the refs modeline indicator."
  :group 'agent-shell)

(defface agent-shell-refs-headerline-face
  '((t :inherit header-line :slant italic))
  "Face for ref snippets in the headerline."
  :group 'agent-shell)

(defface agent-shell-refs-headerline-clip-face
  '((t :inherit font-lock-comment-face))
  "Face for the 📎 icon in the headerline."
  :group 'agent-shell)

(defface agent-shell-refs-headerline-separator-face
  '((t :inherit font-lock-comment-face))
  "Face for the │ separator between snippets."
  :group 'agent-shell)

(defface agent-shell-refs-headerline-more-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the +N more indicator."
  :group 'agent-shell)

;;; --- Capture ---

(defun agent-shell-refs-capture ()
  "Capture the current region as a reference and pulse it."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
        (beg (region-beginning))
        (end (region-end))
        (shell-buf (agent-shell-refs--find-shell-buffer)))
    (unless shell-buf
      (user-error "No agent-shell buffer found"))
    (with-current-buffer shell-buf
      (push text agent-shell-refs--list))
    ;; Pulse feedback
    (pulse-momentary-highlight-region beg end 'highlight)
    ;; Deactivate region
    (deactivate-mark)
    ;; Message
    (let ((count (with-current-buffer shell-buf
                   (length agent-shell-refs--list))))
      (message "📎 Referenced (%d attached)" count))
    ;; Update modeline + headerline
    (with-current-buffer shell-buf
      (agent-shell-refs--update-headerline)
      (force-mode-line-update))))

;;; --- Clear / Remove ---

(defun agent-shell-refs-clear ()
  "Clear all attached references."
  (interactive)
  (let ((buf (agent-shell-refs--find-shell-buffer)))
    (when buf
      (with-current-buffer buf
        (setq agent-shell-refs--list nil)
        (agent-shell-refs--update-headerline)
        (force-mode-line-update))
      (message "📎 Refs cleared"))))

(defun agent-shell-refs-remove ()
  "Pick a reference to remove."
  (interactive)
  (let ((buf (agent-shell-refs--find-shell-buffer)))
    (unless buf (user-error "No agent-shell buffer"))
    (with-current-buffer buf
      (unless agent-shell-refs--list
        (user-error "No refs attached"))
      (let* ((candidates (cl-loop for ref in agent-shell-refs--list
                                  for i from 1
                                  collect (cons (format "%d: %s" i
                                                        (agent-shell-refs--truncate ref 60))
                                                ref)))
             (choice (completing-read "Remove ref: " candidates nil t))
             (ref (cdr (assoc choice candidates))))
        (setq agent-shell-refs--list (delete ref agent-shell-refs--list))
        (agent-shell-refs--update-headerline)
        (force-mode-line-update)
        (message "📎 Removed (%d remaining)" (length agent-shell-refs--list))))))

;;; --- Preview ---

(defun agent-shell-refs-preview ()
  "Show all attached refs in a temporary buffer."
  (interactive)
  (let ((buf (agent-shell-refs--find-shell-buffer)))
    (unless buf (user-error "No agent-shell buffer"))
    (let ((refs (buffer-local-value 'agent-shell-refs--list buf)))
      (if (null refs)
          (message "📎 No refs attached")
        (with-current-buffer (get-buffer-create "*agent-shell-refs*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (cl-loop for ref in (reverse refs)
                     for i from 1
                     do (insert (format "── Ref %d ──\n%s\n\n" i ref)))
            (goto-char (point-min))
            (special-mode))
          (display-buffer (current-buffer)
                          '((display-buffer-below-selected)
                            (window-height . 0.3))))))))

;;; --- Modeline ---

(defun agent-shell-refs--modeline-indicator ()
  "Return modeline string showing ref count, or empty if none."
  (if (and (derived-mode-p 'agent-shell-mode)
           agent-shell-refs--list)
      (propertize (format " 📎%d" (length agent-shell-refs--list))
                  'face 'agent-shell-refs-modeline-face
                  'help-echo (format "%d reference(s) attached — click to preview"
                                     (length agent-shell-refs--list))
                  'mouse-face 'mode-line-highlight
                  'local-map (let ((map (make-sparse-keymap)))
                               (define-key map [mode-line mouse-1]
                                           #'agent-shell-refs-preview)
                               map))
    ""))

(defvar agent-shell-refs--modeline-construct
  '(:eval (agent-shell-refs--modeline-indicator))
  "Mode-line construct for refs indicator.")

(put 'agent-shell-refs--modeline-construct 'risky-local-variable t)

;;; --- Headerline ---

(defvar-local agent-shell-refs--headerline-active nil
  "Non-nil when our headerline is installed.")

(defun agent-shell-refs--truncate (text max-len)
  "Truncate TEXT to MAX-LEN chars, collapsing whitespace, adding … if needed."
  (let ((clean (replace-regexp-in-string "[\n\r\t ]+" " " (string-trim text))))
    (if (<= (length clean) max-len)
        clean
      (concat (substring clean 0 (- max-len 1)) "…"))))

(defun agent-shell-refs--headerline-format ()
  "Build the headerline string from current refs."
  (if (null agent-shell-refs--list)
      nil
    (let* ((refs (reverse agent-shell-refs--list))
           (total (length refs))
           (max-shown 3)
           (shown (seq-take refs max-shown))
           (remaining (- total max-shown))
           (sep (propertize "  │  " 'face 'agent-shell-refs-headerline-separator-face))
           (clip (propertize "📎 " 'face 'agent-shell-refs-headerline-clip-face))
           (snippets (mapcar (lambda (ref)
                               (propertize (agent-shell-refs--truncate ref 25)
                                           'face 'agent-shell-refs-headerline-face))
                             shown))
           (parts (list clip (string-join snippets sep))))
      (when (> remaining 0)
        (setq parts (append parts
                            (list sep
                                  (propertize (format "+%d more" remaining)
                                              'face 'agent-shell-refs-headerline-more-face)))))
      (apply #'concat parts))))

(defun agent-shell-refs--update-headerline ()
  "Set or clear the headerline based on current refs."
  (if agent-shell-refs--list
      (progn
        (setq header-line-format '(:eval (agent-shell-refs--headerline-format)))
        (setq agent-shell-refs--headerline-active t))
    (when agent-shell-refs--headerline-active
      (setq header-line-format nil)
      (setq agent-shell-refs--headerline-active nil))))

;;; --- Submit hook ---

(defun agent-shell-refs--format-for-send (refs)
  "Format REFS list into a context block string."
  (let ((formatted (cl-loop for ref in (reverse refs)
                            for i from 1
                            collect (format "> %s"
                                           (replace-regexp-in-string
                                            "\n" "\n> " ref)))))
    (concat "<referenced-context>\n"
            (mapconcat #'identity formatted "\n\n")
            "\n</referenced-context>\n\n")))

(defun agent-shell-refs--around-submit (orig-fun &rest args)
  "Advice around `shell-maker-submit' to prepend attached refs."
  (when (and (derived-mode-p 'agent-shell-mode)
             agent-shell-refs--list)
    (let ((refs-text (agent-shell-refs--format-for-send agent-shell-refs--list)))
      ;; Find prompt start and prepend refs
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward comint-prompt-regexp nil t)
          (goto-char (match-end 0))
          (insert refs-text)))
      ;; Clear refs
      (setq agent-shell-refs--list nil)
      (agent-shell-refs--update-headerline)
      (force-mode-line-update)))
  (apply orig-fun args))

;;; --- Find shell buffer ---

(defun agent-shell-refs--find-shell-buffer ()
  "Find the appropriate agent-shell buffer for capturing refs.
If we're in an agent-shell buffer, use it.  Otherwise, find the
project's shell buffer."
  (cond
   ((derived-mode-p 'agent-shell-mode) (current-buffer))
   ((and (fboundp 'agent-shell-project-buffers)
         (seq-first (agent-shell-project-buffers))))
   ((and (fboundp 'agent-shell--shell-buffer)
         (ignore-errors (agent-shell--shell-buffer :no-create t))))))

;;; --- Setup / teardown ---

(defun agent-shell-refs-setup ()
  "Enable refs system: install modeline segment and submit advice."
  (interactive)
  ;; Modeline
  (unless (member 'agent-shell-refs--modeline-construct mode-line-misc-info)
    (setq mode-line-misc-info
          (append mode-line-misc-info
                  (list 'agent-shell-refs--modeline-construct))))
  ;; Submit advice
  (advice-add 'shell-maker-submit :around #'agent-shell-refs--around-submit))

(defun agent-shell-refs-teardown ()
  "Disable refs system."
  (interactive)
  (setq mode-line-misc-info
        (delq 'agent-shell-refs--modeline-construct mode-line-misc-info))
  (advice-remove 'shell-maker-submit #'agent-shell-refs--around-submit))

(provide 'agent-shell-refs)

;;; agent-shell-refs.el ends here
