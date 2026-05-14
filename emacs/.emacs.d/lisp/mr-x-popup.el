;;; mr-x-popup.el --- Generic popup prompt with selection and text input -*- lexical-binding: t -*-

;;; Commentary:
;; A reusable popup buffer that appears at the bottom of the frame.
;; Supports option selection (j/k navigation), free-text input, or both.
;; Blocks with recursive-edit until the user responds or cancels.
;;
;; Usage:
;;   ;; Text input only
;;   (mr-x/popup-prompt "What's on your mind?")
;;
;;   ;; Selection from options
;;   (mr-x/popup-prompt "Pick a color" nil '("Red" "Green" "Blue"))
;;
;;   ;; Both, with description
;;   (mr-x/popup-prompt "What's next?" "Pick one or type your own" '("Task A" "Task B"))
;;
;;   ;; Custom header line
;;   (mr-x/popup-prompt "Choose" nil '("A" "B") :header "Weekly Review")

;;; Code:

(require 'cl-lib)

;;; Buffer-local variables

(defvar-local mr-x/popup--result nil
  "Stores the result value to return from the popup.")

(defvar-local mr-x/popup--cancelled nil
  "Non-nil if the popup was cancelled by the user.")

(defvar-local mr-x/popup--options nil
  "List of option strings for selection mode.")

(defvar-local mr-x/popup--selected-index 0
  "Currently selected option index (0-based).")

(defvar-local mr-x/popup--selection-overlay nil
  "Overlay used to highlight the currently selected option.")

(defvar-local mr-x/popup--text-start nil
  "Marker for start of editable text region.")

(defvar-local mr-x/popup--text-end nil
  "Marker for end of editable text region.")

(defvar-local mr-x/popup--field-overlay nil
  "Overlay for the text field with its own local-map.")

;;; Interactive commands

(defun mr-x/popup-cancel ()
  "Cancel the popup and return nil."
  (interactive)
  (setq mr-x/popup--cancelled t)
  (setq mr-x/popup--result nil)
  (message "Cancelled")
  (exit-recursive-edit))

(defun mr-x/popup--submit-text ()
  "Submit text content from editable region and exit."
  (interactive)
  (if mr-x/popup--text-start
      (let* ((text-content (buffer-substring-no-properties
                            mr-x/popup--text-start
                            mr-x/popup--text-end))
             (trimmed (string-trim text-content)))
        (if (and (string-empty-p trimmed) mr-x/popup--options)
            ;; Empty text and options exist — confirm current option instead
            (mr-x/popup--confirm-selection)
          (setq mr-x/popup--result trimmed)
          (exit-recursive-edit)))
    (mr-x/popup-cancel)))

(defun mr-x/popup--insert-newline ()
  "Insert newline in text field."
  (interactive)
  (when (and mr-x/popup--text-start
             (>= (point) (marker-position mr-x/popup--text-start))
             (<= (point) (marker-position mr-x/popup--text-end)))
    (insert "\n")))

(defun mr-x/popup--handle-return ()
  "Handle RET: in text field submit text, in options confirm selection."
  (interactive)
  (if (and mr-x/popup--text-start
           (>= (point) (marker-position mr-x/popup--text-start))
           (<= (point) (marker-position mr-x/popup--text-end)))
      (mr-x/popup--submit-text)
    (mr-x/popup--confirm-selection)))

;;; Navigation functions

(defun mr-x/popup--move-selection-overlay ()
  "Move selection overlay to the currently selected option line."
  (when (and mr-x/popup--selection-overlay mr-x/popup--options)
    (let ((target-pos (text-property-any (point-min) (point-max)
                                         'option-index
                                         mr-x/popup--selected-index)))
      (when target-pos
        (save-excursion
          (goto-char target-pos)
          (move-overlay mr-x/popup--selection-overlay
                        (line-beginning-position)
                        (1+ (line-end-position))))))))

(defun mr-x/popup--select-next ()
  "Move selection to next option with wrap-around."
  (interactive)
  (when mr-x/popup--options
    (setq mr-x/popup--selected-index
          (mod (1+ mr-x/popup--selected-index)
               (length mr-x/popup--options)))
    (mr-x/popup--move-selection-overlay)
    (let ((pos (text-property-any (point-min) (point-max)
                                  'option-index mr-x/popup--selected-index)))
      (when pos (goto-char pos)))))

(defun mr-x/popup--select-prev ()
  "Move selection to previous option with wrap-around."
  (interactive)
  (when mr-x/popup--options
    (setq mr-x/popup--selected-index
          (mod (1- mr-x/popup--selected-index)
               (length mr-x/popup--options)))
    (mr-x/popup--move-selection-overlay)
    (let ((pos (text-property-any (point-min) (point-max)
                                  'option-index mr-x/popup--selected-index)))
      (when pos (goto-char pos)))))

(defun mr-x/popup--confirm-selection ()
  "Confirm the current selection and exit."
  (interactive)
  (when mr-x/popup--options
    (let ((selected-option (nth mr-x/popup--selected-index mr-x/popup--options)))
      (setq mr-x/popup--result selected-option)
      (exit-recursive-edit))))

(defun mr-x/popup--select-option-at-point ()
  "Select and confirm the option at point (for mouse support)."
  (interactive)
  (when mr-x/popup--options
    (let ((idx (get-text-property (point) 'option-index)))
      (when (and idx (>= idx 0) (< idx (length mr-x/popup--options)))
        (setq mr-x/popup--selected-index idx)
        (mr-x/popup--confirm-selection)))))

(defun mr-x/popup--jump-to-field ()
  "Jump to the text input field and enter insert state."
  (interactive)
  (when mr-x/popup--text-start
    (goto-char mr-x/popup--text-start)
    ;; Dim selection highlight
    (when mr-x/popup--selection-overlay
      (overlay-put mr-x/popup--selection-overlay
                   'face '(:inverse-video t :foreground "gray50")))
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))))

(defun mr-x/popup--jump-to-options ()
  "Jump back to the options list from the text field."
  (interactive)
  (when mr-x/popup--options
    ;; Restore selection highlight
    (when mr-x/popup--selection-overlay
      (overlay-put mr-x/popup--selection-overlay 'face '(:inverse-video t)))
    (mr-x/popup--move-selection-overlay)
    (let ((pos (text-property-any (point-min) (point-max)
                                  'option-index mr-x/popup--selected-index)))
      (when pos (goto-char pos)))
    (when (fboundp 'evil-normal-state)
      (evil-normal-state))))

;;; Keymaps

;; Buffer-wide keymap: suppress all self-insert, bind navigation keys.
;; Active in the read-only options region.
(defvar mr-x/popup-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "C-g") #'mr-x/popup-cancel)
    (define-key map (kbd "q") #'mr-x/popup-cancel)
    (define-key map (kbd "j") #'mr-x/popup--select-next)
    (define-key map (kbd "k") #'mr-x/popup--select-prev)
    (define-key map (kbd "C-n") #'mr-x/popup--select-next)
    (define-key map (kbd "C-p") #'mr-x/popup--select-prev)
    (define-key map (kbd "<down>") #'mr-x/popup--select-next)
    (define-key map (kbd "<up>") #'mr-x/popup--select-prev)
    (define-key map (kbd "RET") #'mr-x/popup--handle-return)
    (define-key map (kbd "<return>") #'mr-x/popup--handle-return)
    (define-key map (kbd "<mouse-1>") #'mr-x/popup--select-option-at-point)
    (define-key map (kbd "TAB") #'mr-x/popup--jump-to-field)
    (define-key map (kbd "<tab>") #'mr-x/popup--jump-to-field)
    (define-key map (kbd "i") #'mr-x/popup--jump-to-field)
    map)
  "Keymap for `mr-x/popup-mode'.")

;; Text field keymap: sparse, so unbound keys self-insert via global-map.
(defvar mr-x/popup-field-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'mr-x/popup-cancel)
    (define-key map (kbd "RET") #'mr-x/popup--submit-text)
    (define-key map (kbd "<return>") #'mr-x/popup--submit-text)
    (define-key map (kbd "C-c C-c") #'mr-x/popup--submit-text)
    (define-key map (kbd "C-j") #'mr-x/popup--insert-newline)
    (define-key map (kbd "S-RET") #'mr-x/popup--insert-newline)
    (define-key map (kbd "S-<return>") #'mr-x/popup--insert-newline)
    (define-key map (kbd "TAB") #'mr-x/popup--jump-to-options)
    (define-key map (kbd "<tab>") #'mr-x/popup--jump-to-options)
    map)
  "Keymap for the text input field overlay.")

;;; Evil integration

(with-eval-after-load 'evil
  (evil-set-initial-state 'mr-x/popup-mode 'normal)

  (evil-define-key 'normal mr-x/popup-mode-map
    (kbd "j") #'mr-x/popup--select-next
    (kbd "k") #'mr-x/popup--select-prev
    (kbd "C-n") #'mr-x/popup--select-next
    (kbd "C-p") #'mr-x/popup--select-prev
    (kbd "G") #'end-of-buffer
    (kbd "gg") #'beginning-of-buffer
    (kbd "RET") #'mr-x/popup--handle-return
    (kbd "<return>") #'mr-x/popup--handle-return
    (kbd "q") #'mr-x/popup-cancel
    (kbd "TAB") #'mr-x/popup--jump-to-field
    (kbd "<tab>") #'mr-x/popup--jump-to-field
    (kbd "i") #'mr-x/popup--jump-to-field
    (kbd "<escape>") #'ignore)

  (evil-define-key 'insert mr-x/popup-mode-map
    (kbd "C-c C-c") #'mr-x/popup--submit-text
    (kbd "C-j") #'mr-x/popup--insert-newline
    (kbd "<escape>") (lambda () (interactive)
                       (when (fboundp 'evil-normal-state)
                         (evil-normal-state))
                       (mr-x/popup--jump-to-options))))

;;; Major mode

(define-derived-mode mr-x/popup-mode fundamental-mode "Popup"
  "Major mode for popup prompt buffers.
\\{mr-x/popup-mode-map}"
  (setq-local mode-line-format nil)
  (setq-local cursor-type 'bar)
  (add-hook 'before-change-functions #'mr-x/popup--before-change nil t))

(defun mr-x/popup--before-change (beg end)
  "Prevent edits outside the text field region."
  (when (and mr-x/popup--text-start
             (not inhibit-read-only)
             (or (< beg (marker-position mr-x/popup--text-start))
                 (> end (marker-position mr-x/popup--text-end))))
    (signal 'text-read-only '("Cannot edit outside the text field"))))

;;; Markdown rendering

(defun mr-x/popup--render-markdown (start end)
  "Apply basic markdown styling to text between START and END."
  (save-excursion
    ;; Bold: **text**
    (goto-char start)
    (while (re-search-forward "\\*\\*\\(.+?\\)\\*\\*" end t)
      (let ((content (match-string 1))
            (m-start (match-beginning 0)))
        (replace-match content t t)
        (setq end (- end 4))
        (put-text-property m-start (+ m-start (length content))
                           'face '(:weight bold))))
    ;; Italic: *text*
    (goto-char start)
    (while (re-search-forward "\\(?:^\\|[[:space:](]\\)\\(\\*\\(.+?\\)\\*\\)\\(?:[[:space:].,;:!?)]\\|$\\)" end t)
      (let ((content (match-string 2))
            (m-start (match-beginning 1))
            (m-end (match-end 1)))
        (save-excursion
          (goto-char m-start)
          (delete-region m-start m-end)
          (insert content)
          (setq end (- end 2))
          (put-text-property m-start (+ m-start (length content))
                             'face '(:slant italic)))))
    ;; Inline code: `text`
    (goto-char start)
    (while (re-search-forward "`\\([^`\n]+?\\)`" end t)
      (let ((content (match-string 1))
            (m-start (match-beginning 0)))
        (replace-match content t t)
        (setq end (- end 2))
        (put-text-property m-start (+ m-start (length content))
                           'face '(:family "monospace" :background "#2a2a2a"))))))

;;; Main popup function

(cl-defun mr-x/popup-prompt (question &optional description options &key header)
  "Display QUESTION in a popup buffer at the bottom of the frame.
Returns the user's response string, or nil if cancelled.

DESCRIPTION is optional context text displayed below the question.
OPTIONS is an optional list of strings for selection mode.
HEADER is an optional string for the header line (defaults to question).

When OPTIONS is provided, shows a numbered list with j/k navigation.
A text field is always available below (TAB or i to jump to it).

When OPTIONS is nil, only shows a text field for free-form input.

Blocks until the user responds (RET) or cancels (C-g/q).
Returns nil if cancelled (does not signal an error)."
  (let* ((buf (get-buffer-create "*mr-x-popup*"))
         (win nil)
         (result nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (remove-overlays)
              (mr-x/popup-mode)

              ;; Header line
              (setq-local header-line-format
                          (propertize (concat " " (or header question))
                                      'face '(:weight bold)))

              ;; Question
              (insert (propertize question 'face '(:weight bold)))
              (insert "\n")

              ;; Description with markdown rendering
              (when description
                (let ((desc-start (point)))
                  (insert (propertize description 'face '(:height 0.9)))
                  (mr-x/popup--render-markdown desc-start (point)))
                (insert "\n"))

              ;; Separator
              (insert (propertize (make-string 40 ?─) 'face 'shadow))
              (insert "\n")

              ;; Options
              (when options
                (setq mr-x/popup--options options)
                (setq mr-x/popup--selected-index 0)

                (let ((idx 0))
                  (dolist (option options)
                    (let ((start (point)))
                      (insert (format "%d. %s\n" (1+ idx) option))
                      (put-text-property start (point) 'option-index idx))
                    (setq idx (1+ idx))))

                (insert "\n")

                ;; Selection overlay
                (setq mr-x/popup--selection-overlay (make-overlay 1 1))
                (overlay-put mr-x/popup--selection-overlay 'face '(:inverse-video t))
                (save-excursion
                  (let ((pos (text-property-any (point-min) (point-max)
                                                'option-index 0)))
                    (when pos
                      (goto-char pos)
                      (move-overlay mr-x/popup--selection-overlay
                                    (line-beginning-position)
                                    (1+ (line-end-position)))))))

              ;; Text input field
              (goto-char (point-max))

              (when options
                (insert (propertize (make-string 40 ?─) 'face 'shadow))
                (insert "\n"))

              (insert (propertize (if options "Or type a response:" "Type your response:")
                                  'face 'shadow))
              (insert "\n")

              ;; Editable text region
              (setq mr-x/popup--text-start (point-marker))
              (set-marker-insertion-type mr-x/popup--text-start nil)
              (insert "\n")
              (setq mr-x/popup--text-end (point-marker))
              (set-marker-insertion-type mr-x/popup--text-end t)

              ;; Field overlay with local-map
              (setq mr-x/popup--field-overlay
                    (make-overlay (marker-position mr-x/popup--text-start)
                                  (marker-position mr-x/popup--text-end)
                                  nil nil t))
              (overlay-put mr-x/popup--field-overlay 'local-map mr-x/popup-field-map)
              (overlay-put mr-x/popup--field-overlay 'face '(:extend t))

              ;; Reset state
              (setq mr-x/popup--result nil)
              (setq mr-x/popup--cancelled nil)))

          ;; Display at bottom
          (setq win (display-buffer buf
                                    '((display-buffer-at-bottom)
                                      (window-height . 0.6)
                                      (preserve-size . (nil . t)))))
          (select-window win)

          ;; Position cursor
          (if (buffer-local-value 'mr-x/popup--options buf)
              (progn
                (goto-char (point-min))
                (let ((pos (text-property-any (point-min) (point-max) 'option-index 0)))
                  (when pos (goto-char pos))))
            (goto-char (buffer-local-value 'mr-x/popup--text-start buf))
            (when (fboundp 'evil-insert-state)
              (evil-insert-state)))

          (set-window-start win (point-min))

          ;; Block until response
          (recursive-edit)

          ;; Return result (nil if cancelled)
          (setq result (buffer-local-value 'mr-x/popup--result buf)))

      ;; Cleanup
      (when (buffer-live-p buf)
        (when (get-buffer-window buf)
          (quit-window t (get-buffer-window buf)))
        (when (buffer-live-p buf)
          (kill-buffer buf))))
    result))

(provide 'mr-x-popup)

;;; mr-x-popup.el ends here
