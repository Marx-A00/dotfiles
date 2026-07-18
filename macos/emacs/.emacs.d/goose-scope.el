;;; goose-scope.el --- Live view of the local Ollama agent stack -*- lexical-binding: t; -*-

;; Tails the goose-scope proxy's NDJSON event log and renders each agent
;; "turn" as a timeline: which models fired, in what order, how many tokens,
;; how fast, and where the time actually went (generation vs model load/swap).
;;
;; Data source: ~/.dotfiles/macos/scripts/goose-scope-proxy.py — a transparent
;; logging proxy that sits in front of Ollama and records one NDJSON event per
;; /api/chat call. See ~/roaming/projects/home-lab/docs/goose-ollama-setup.md.
;;
;; Usage:  M-x goose-scope   (opens the *goose-scope* panel, live-updating)

;;; Code:

(require 'json)
(require 'subr-x)
(require 'filenotify)

(defgroup goose-scope nil
  "Live observability for the local Ollama agent stack."
  :group 'tools)

(defcustom goose-scope-log-file
  (expand-file-name "~/.local/state/goose-scope/events.ndjson")
  "NDJSON event log written by the goose-scope proxy."
  :type 'file :group 'goose-scope)

(defcustom goose-scope-turn-gap 20
  "Seconds of idle between LLM calls that begins a new turn."
  :type 'number :group 'goose-scope)

(defcustom goose-scope-max-turns 30
  "Render at most this many of the most recent turns."
  :type 'integer :group 'goose-scope)

(defcustom goose-scope-bar-width 18
  "Maximum width, in characters, of the timing bar."
  :type 'integer :group 'goose-scope)

(defcustom goose-scope-slow-load-ms 1000
  "A load_duration above this (ms) is flagged as a cold load / model swap."
  :type 'number :group 'goose-scope)

(defcustom goose-scope-primary-models
  '("qwen3.5:9b" "qwen3.5:9b-notk" "gemma4:12b")
  "Models treated as the reasoning/primary role (everything else = toolshim parser)."
  :type '(repeat string) :group 'goose-scope)

;; ── Faces (gruvbox) ────────────────────────────────────────────
(defface goose-scope-header  '((t :foreground "#fabd2f" :weight bold)) "Turn header.")
(defface goose-scope-primary '((t :foreground "#8ec07c")) "Primary/reasoning model.")
(defface goose-scope-shim    '((t :foreground "#fe8019")) "Toolshim parser model.")
(defface goose-scope-tool    '((t :foreground "#d3869b" :weight bold)) "Tool call.")
(defface goose-scope-dim     '((t :foreground "#928374")) "Dim/secondary text.")
(defface goose-scope-warn    '((t :foreground "#fb4934" :weight bold)) "Slow-load warning.")
(defface goose-scope-bar     '((t :foreground "#83a598")) "Timing bar.")
(defface goose-scope-prompt  '((t :foreground "#ebdbb2" :slant italic)) "User prompt text.")

(defconst goose-scope--bar-blocks ["" "▏" "▎" "▍" "▌" "▋" "▊" "▉"])

(defcustom goose-scope-proxy-script
  (expand-file-name "~/.dotfiles/macos/scripts/goose-scope-proxy.py")
  "Path to the goose-scope logging proxy script."
  :type 'file :group 'goose-scope)

(defcustom goose-scope-proxy-port 11435
  "Local port the proxy listens on (must match config.yaml OLLAMA_HOST)."
  :type 'integer :group 'goose-scope)

(defvar goose-scope--watch nil "file-notify descriptor for the log file.")
(defvar goose-scope--proxy-process nil "Handle to the proxy we started, if any.")
(defconst goose-scope-buffer-name "*goose-scope*")

(defun goose-scope--proxy-running-p ()
  "Non-nil if something is already listening on the proxy port."
  (or (process-live-p goose-scope--proxy-process)
      (condition-case nil
          (let ((p (make-network-process :name "goose-scope-probe" :host "127.0.0.1"
                                         :service goose-scope-proxy-port :nowait nil)))
            (delete-process p) t)
        (error nil))))

(defun goose-scope-ensure-proxy ()
  "Start the goose-scope proxy if it isn't already running.
config.yaml points goose at this proxy, so it needs to be up for goose to
reach Ollama.  Started as an Emacs subprocess (lives as long as the daemon)."
  (interactive)
  (if (goose-scope--proxy-running-p)
      (when (called-interactively-p 'interactive)
        (message "goose-scope: proxy already running on :%d" goose-scope-proxy-port))
    (if (not (file-exists-p goose-scope-proxy-script))
        (message "goose-scope: proxy script not found at %s" goose-scope-proxy-script)
      (setq goose-scope--proxy-process
            (start-process "goose-scope-proxy" " *goose-scope-proxy*"
                           "python3" goose-scope-proxy-script))
      (message "goose-scope: started proxy on :%d -> Ollama" goose-scope-proxy-port))))

;; ── Parsing ────────────────────────────────────────────────────
(defun goose-scope--read-events ()
  "Parse the NDJSON log into a list of plists, oldest first."
  (when (file-readable-p goose-scope-log-file)
    (with-temp-buffer
      (insert-file-contents goose-scope-log-file)
      (let (events)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                      (line-end-position)))))
            (unless (string-empty-p line)
              (ignore-errors
                (push (json-parse-string line :object-type 'plist
                                         :null-object nil :false-object nil)
                      events))))
          (forward-line 1))
        (nreverse events)))))

(defun goose-scope--group-turns (events)
  "Group EVENTS (oldest first) into a list of turns (each a list of events)."
  (let (turns cur last-ts)
    (dolist (ev events)
      (let ((ts (plist-get ev :ts)))
        (when (and last-ts cur (> (- ts last-ts) goose-scope-turn-gap))
          (push (nreverse cur) turns)
          (setq cur nil))
        (push ev cur)
        (setq last-ts ts)))
    (when cur (push (nreverse cur) turns))
    (nreverse turns)))

;; ── Rendering helpers ──────────────────────────────────────────
(defun goose-scope--primary-p (model)
  (member model goose-scope-primary-models))

(defun goose-scope--bar (ms max-ms)
  "A sub-character-precise bar for MS relative to MAX-MS."
  (if (or (not ms) (not max-ms) (<= max-ms 0))
      ""
    (let* ((units (* goose-scope-bar-width 8 (/ (float ms) max-ms)))
           (full (floor (/ units 8)))
           (rem (floor (mod units 8))))
      (concat (make-string (min full goose-scope-bar-width) ?█)
              (when (< full goose-scope-bar-width)
                (aref goose-scope--bar-blocks rem))))))

(defun goose-scope--ms (ms)
  (cond ((null ms) "—")
        ((>= ms 1000) (format "%.1fs" (/ ms 1000.0)))
        (t (format "%dms" (round ms)))))

(defun goose-scope--hms (ts)
  (format-time-string "%H:%M:%S" (seconds-to-time ts)))

(defun goose-scope--clean (s)
  "Collapse S to a single tidy display line, stripping goose's wrappers."
  (when s
    (let ((s (replace-regexp-in-string
              "-\\{2,\\}\\(BEGIN\\|END\\) USER MESSAGES-\\{2,\\}" "" s)))
      (setq s (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " s)))
      ;; Continuation calls carry a tool result back to the model — label them.
      (if (string-prefix-p "Tool result:" s) "↩ (tool result)" s))))

(defun goose-scope--turn-prompt (turn)
  "First meaningful user prompt in TURN (not a tool-result continuation)."
  (or (seq-some (lambda (ev)
                  (let ((u (goose-scope--clean (plist-get ev :last_user))))
                    (and u (not (string-empty-p u))
                         (not (string-prefix-p "↩" u)) u)))
                turn)
      (goose-scope--clean (plist-get (car turn) :last_user))
      "(continuation)"))

(defun goose-scope--insert-turn (turn max-ms)
  (let* ((first (car turn))
         (last (car (last turn)))
         (t0 (plist-get first :ts))
         (t1 (+ (plist-get last :ts) (/ (or (plist-get last :wall_ms) 0) 1000.0)))
         (span (- t1 t0))
         (prompt (goose-scope--turn-prompt turn))
         (ncalls (length turn)))
    ;; header
    (insert (propertize "● " 'face 'goose-scope-header)
            (propertize (goose-scope--hms t0) 'face 'goose-scope-header)
            "  "
            (propertize (truncate-string-to-width prompt 60 nil nil "…")
                        'face 'goose-scope-prompt)
            (propertize (format "   (%.1fs · %d call%s)" span ncalls
                                (if (= ncalls 1) "" "s"))
                        'face 'goose-scope-dim)
            "\n")
    ;; call rows
    (let ((i 0))
      (dolist (ev turn)
        (setq i (1+ i))
        (let* ((lastp (= i ncalls))
               (model (or (plist-get ev :model) "?"))
               (primp (goose-scope--primary-p model))
               (eval-ms (plist-get ev :eval_ms))
               (wall-ms (plist-get ev :wall_ms))
               (time-ms (or eval-ms wall-ms))  ; /v1 has no eval breakdown
               (approx (null eval-ms))
               (load-ms (plist-get ev :load_ms))
               (otok (plist-get ev :output_tokens))
               (toks (plist-get ev :tok_s))
               (tool (plist-get ev :tool_call)))
          (insert (propertize (if lastp "  └─ " "  ├─ ") 'face 'goose-scope-dim)
                  (propertize (format "%-16s" model)
                              'face (if primp 'goose-scope-primary 'goose-scope-shim))
                  (propertize (format "%7s " (concat (if approx "~" "")
                                                     (goose-scope--ms time-ms)))
                              'face 'goose-scope-dim)
                  (propertize (goose-scope--bar time-ms max-ms) 'face 'goose-scope-bar))
          ;; token/rate column
          (when otok
            (insert (propertize (format "  %d tok" otok) 'face 'goose-scope-dim)))
          (when toks
            (insert (propertize (format " · %.0f tok/s" toks) 'face 'goose-scope-dim)))
          ;; tool-call marker
          (when tool
            (insert "  " (propertize "🔧 tool" 'face 'goose-scope-tool)))
          ;; slow-load warning
          (when (and load-ms (> load-ms goose-scope-slow-load-ms))
            (insert "  " (propertize (format "⚠ load %s" (goose-scope--ms load-ms))
                                     'face 'goose-scope-warn)))
          (insert "\n"))))
    (insert "\n")))

(defun goose-scope--render ()
  "Redraw the *goose-scope* buffer from the log."
  (let ((buf (get-buffer goose-scope-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (events (goose-scope--read-events))
               (turns (goose-scope--group-turns events))
               (turns (last turns goose-scope-max-turns))
               (max-ms (apply #'max 1 (delq nil (mapcar (lambda (e)
                                                          (or (plist-get e :eval_ms)
                                                              (plist-get e :wall_ms)))
                                                        events))))
               (pt (point)))
          (erase-buffer)
          (insert (propertize " goose-scope " 'face '(:background "#fabd2f" :foreground "#1d2021" :weight bold))
                  (propertize (format "  %d turns · %d calls · tailing %s\n\n"
                                      (length turns) (length events)
                                      (abbreviate-file-name goose-scope-log-file))
                              'face 'goose-scope-dim))
          (if (null turns)
              (insert (propertize "  (no events yet — fire a prompt through the proxy)\n"
                                  'face 'goose-scope-dim))
            (dolist (turn turns)
              (goose-scope--insert-turn turn max-ms)))
          (goto-char (min pt (point-max))))))))

;; ── Live tailing ───────────────────────────────────────────────
(defun goose-scope--on-change (&rest _)
  (when (get-buffer goose-scope-buffer-name)
    (goose-scope--render)))

(defun goose-scope--start-watch ()
  (goose-scope--stop-watch)
  (when (file-exists-p goose-scope-log-file)
    (setq goose-scope--watch
          (ignore-errors
            (file-notify-add-watch goose-scope-log-file '(change)
                                   #'goose-scope--on-change)))))

(defun goose-scope--stop-watch ()
  (when goose-scope--watch
    (ignore-errors (file-notify-rm-watch goose-scope--watch))
    (setq goose-scope--watch nil)))

;; ── Mode + command ─────────────────────────────────────────────
(defvar goose-scope-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'goose-scope-refresh)
    (define-key m (kbd "q") #'quit-window)
    m)
  "Keymap for `goose-scope-mode'.")

(define-derived-mode goose-scope-mode special-mode "GooseScope"
  "Major mode for the live goose-scope panel."
  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (add-hook 'kill-buffer-hook #'goose-scope--stop-watch nil t))

(defun goose-scope-refresh ()
  "Re-read the log and redraw."
  (interactive)
  (goose-scope--render))

;;;###autoload
(defun goose-scope ()
  "Open the live *goose-scope* observability panel."
  (interactive)
  (goose-scope-ensure-proxy)
  (let ((buf (get-buffer-create goose-scope-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'goose-scope-mode)
        (goose-scope-mode))
      (goose-scope--start-watch))
    (goose-scope--render)
    (display-buffer buf)
    buf))

(provide 'goose-scope)
;;; goose-scope.el ends here
