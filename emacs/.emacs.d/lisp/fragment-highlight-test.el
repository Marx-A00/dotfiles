;;; fragment-highlight-test.el --- Test harness for fragment syntax highlighting -*- lexical-binding: t -*-

;; Author: Marcos Andrade
;; Description: Standalone test environment for developing fragment code highlighting
;;              without requiring a live agent-shell session.

;;; Commentary:
;;
;; This file provides a test harness to develop and iterate on syntax highlighting
;; for collapsible fragments. It simulates the fragment structure that agent-shell
;; creates, allowing rapid testing of highlighting logic.
;;
;; Usage:
;;   M-x mr-x/fragment-test-harness    - Open test buffer with sample fragments
;;   M-x mr-x/fragment-test-highlight  - Apply highlighting to current test buffer
;;   M-x mr-x/fragment-test-reset      - Reset test buffer to unhighlighted state
;;   M-x mr-x/fragment-test-toggle     - Toggle fragment expanded/collapsed
;;
;; The test buffer simulates:
;;   - Collapsible indicators (▼/▶)
;;   - Labels (tool name, status)
;;   - Body content with embedded code blocks
;;   - Text properties mimicking agent-shell-ui

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Sample Content - Various languages and scenarios
;;; ============================================================================

(defvar mr-x/fragment-test-samples
  '((:tool "Read"
     :file "/src/components/Button.tsx"
     :status "done"
     :body "```typescript
import React from 'react';

interface ButtonProps {
  label: string;
  onClick: () => void;
  disabled?: boolean;
}

export const Button: React.FC<ButtonProps> = ({ label, onClick, disabled }) => {
  return (
    <button
      className=\"btn btn-primary\"
      onClick={onClick}
      disabled={disabled}
    >
      {label}
    </button>
  );
};
```")

    (:tool "Read"
     :file "/src/utils/api.py"
     :status "done"
     :body "```python
import asyncio
from typing import Optional, Dict, Any
import httpx

class APIClient:
    \"\"\"Async HTTP client for API requests.\"\"\"
    
    def __init__(self, base_url: str, timeout: int = 30):
        self.base_url = base_url
        self.timeout = timeout
        self._client: Optional[httpx.AsyncClient] = None
    
    async def get(self, endpoint: str, params: Optional[Dict] = None) -> Any:
        async with httpx.AsyncClient() as client:
            response = await client.get(
                f\"{self.base_url}/{endpoint}\",
                params=params,
                timeout=self.timeout
            )
            response.raise_for_status()
            return response.json()
```")

    (:tool "Bash"
     :file "grep -n 'export' src/"
     :status "done"  
     :body "src/components/Button.tsx:15:export const Button: React.FC<ButtonProps> = ({ label, onClick, disabled }) => {
src/components/Modal.tsx:42:export const Modal = ({ isOpen, onClose, children }) => {
src/utils/helpers.ts:8:export function debounce<T extends (...args: any[]) => any>(
src/utils/helpers.ts:23:export function throttle<T extends (...args: any[]) => any>(
src/index.ts:1:export * from './components';
src/index.ts:2:export * from './utils';")

    (:tool "Read"
     :file "/src/config/database.go"
     :status "done"
     :body "```go
package config

import (
    \"context\"
    \"database/sql\"
    \"fmt\"
    \"time\"
    
    _ \"github.com/lib/pq\"
)

type DBConfig struct {
    Host     string
    Port     int
    User     string
    Password string
    DBName   string
}

func NewConnection(cfg DBConfig) (*sql.DB, error) {
    dsn := fmt.Sprintf(
        \"host=%s port=%d user=%s password=%s dbname=%s sslmode=disable\",
        cfg.Host, cfg.Port, cfg.User, cfg.Password, cfg.DBName,
    )
    
    db, err := sql.Open(\"postgres\", dsn)
    if err != nil {
        return nil, fmt.Errorf(\"failed to open database: %w\", err)
    }
    
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()
    
    if err := db.PingContext(ctx); err != nil {
        return nil, fmt.Errorf(\"failed to ping database: %w\", err)
    }
    
    return db, nil
}
```")

    (:tool "Thinking"
     :file nil
     :status nil
     :body "I need to analyze the user's request for implementing authentication.

Let me consider the options:

```typescript
// Option 1: JWT-based auth
interface JWTPayload {
  userId: string;
  email: string;
  exp: number;
}

const verifyToken = (token: string): JWTPayload => {
  return jwt.verify(token, process.env.JWT_SECRET) as JWTPayload;
};
```

Or we could use session-based:

```python
# Option 2: Session-based auth
from flask import session
from functools import wraps

def login_required(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        if 'user_id' not in session:
            return redirect(url_for('login'))
        return f(*args, **kwargs)
    return decorated_function
```

I think JWT would be better for this API-first architecture.")

    (:tool "Read"
     :file "/init.el"
     :status "done"
     :body "```emacs-lisp
;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

(require 'package)
(setq package-archives
      '((\"melpa\" . \"https://melpa.org/packages/\")
        (\"gnu\" . \"https://elpa.gnu.org/packages/\")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package magit
  :ensure t
  :bind ((\"C-x g\" . magit-status)))
```")

    (:tool "Bash"
     :file "cat package.json"
     :status "done"
     :body "```json
{
  \"name\": \"my-app\",
  \"version\": \"1.0.0\",
  \"scripts\": {
    \"dev\": \"next dev\",
    \"build\": \"next build\",
    \"start\": \"next start\",
    \"lint\": \"eslint . --ext .ts,.tsx\",
    \"test\": \"jest --coverage\"
  },
  \"dependencies\": {
    \"next\": \"^14.0.0\",
    \"react\": \"^18.2.0\",
    \"react-dom\": \"^18.2.0\"
  },
  \"devDependencies\": {
    \"@types/node\": \"^20.0.0\",
    \"@types/react\": \"^18.2.0\",
    \"typescript\": \"^5.0.0\"
  }
}
```"))
  "Sample fragments for testing highlighting.")

;;; ============================================================================
;;; Fragment Rendering (simulates agent-shell-ui)
;;; ============================================================================

(defvar-local mr-x/fragment-test-fragments nil
  "List of fragment data in current test buffer.")

(defun mr-x/fragment-test--render-fragment (fragment &optional collapsed)
  "Render a single FRAGMENT. If COLLAPSED, hide body."
  (let* ((tool (plist-get fragment :tool))
         (file (plist-get fragment :file))
         (status (plist-get fragment :status))
         (body (plist-get fragment :body))
         (indicator (if collapsed "▶ " "▼ "))
         (start (point))
         label-start label-end body-start body-end)
    
    ;; Insert indicator
    (insert (propertize indicator
                        'face '(:foreground "#fabd2f")
                        'mr-x-fragment-indicator t
                        'mr-x-fragment-id (length mr-x/fragment-test-fragments)))
    
    ;; Insert label
    (setq label-start (point))
    (insert (propertize (format "%s %s" tool (or file ""))
                        'face '(:foreground "#83a598" :weight bold)))
    (when status
      (insert (propertize (format " [%s]" status)
                          'face '(:foreground "#b8bb26"))))
    (setq label-end (point))
    (insert "\n")
    
    ;; Insert body
    (setq body-start (point))
    (let ((indented-body (replace-regexp-in-string "^" "  " body)))
      (insert (if collapsed
                  (propertize indented-body 'invisible t)
                indented-body)))
    (setq body-end (point))
    (insert "\n\n")
    
    ;; Store fragment info
    (push (list :start start
                :label-start label-start
                :label-end label-end
                :body-start body-start
                :body-end body-end
                :collapsed collapsed
                :data fragment)
          mr-x/fragment-test-fragments)))

(defun mr-x/fragment-test--render-all ()
  "Render all sample fragments."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq mr-x/fragment-test-fragments nil)
    
    ;; Header
    (insert (propertize "Fragment Highlight Test Harness\n" 
                        'face '(:height 1.3 :weight bold)))
    (insert (propertize "================================\n\n"
                        'face '(:foreground "#665c54")))
    (insert "Commands:\n")
    (insert "  C-c h   - Apply highlighting to all fragments\n")
    (insert "  C-c r   - Reset (remove highlighting)\n")
    (insert "  C-c t   - Toggle fragment at point\n")
    (insert "  TAB     - Toggle fragment at point\n")
    (insert "  C-c n   - Next fragment\n")
    (insert "  C-c p   - Previous fragment\n")
    (insert "\n")
    (insert (propertize (make-string 60 ?─) 'face '(:foreground "#665c54")))
    (insert "\n\n")
    
    ;; Render each sample
    (dolist (sample mr-x/fragment-test-samples)
      (mr-x/fragment-test--render-fragment sample))
    
    ;; Reverse to get correct order
    (setq mr-x/fragment-test-fragments (nreverse mr-x/fragment-test-fragments))
    
    (goto-char (point-min))))

;;; ============================================================================
;;; Highlighting Implementation (the actual feature we're developing)
;;; ============================================================================

(defvar mr-x/fragment-highlight-language-mapping
  '(("elisp" . "emacs-lisp")
    ("emacs-lisp" . "emacs-lisp")
    ("typescript" . "typescript-ts")
    ("tsx" . "tsx-ts")
    ("javascript" . "js-ts")
    ("js" . "js-ts")
    ("python" . "python-ts")
    ("py" . "python-ts")
    ("rust" . "rust-ts")
    ("go" . "go-ts")
    ("golang" . "go-ts")
    ("bash" . "bash-ts")
    ("sh" . "bash-ts")
    ("shell" . "bash-ts")
    ("json" . "json-ts")
    ("yaml" . "yaml-ts")
    ("css" . "css-ts")
    ("html" . "html-ts")
    ("sql" . "sql")
    ("c" . "c-ts")
    ("cpp" . "c++-ts")
    ("c++" . "c++-ts"))
  "Map language identifiers to Emacs mode base names.")

(defun mr-x/fragment-highlight--resolve-mode (language)
  "Resolve LANGUAGE string to an Emacs major mode function."
  (when (and language (not (string-empty-p language)))
    (let* ((lang-lower (downcase (string-trim language)))
           (mapped (or (cdr (assoc lang-lower mr-x/fragment-highlight-language-mapping))
                       lang-lower))
           (ts-mode (intern (concat mapped "-mode")))
           (std-mode (intern (concat mapped "-mode"))))
      (cond
       ((fboundp ts-mode) ts-mode)
       ((fboundp std-mode) std-mode)
       (t nil)))))

(defun mr-x/fragment-highlight--fontify-string (text mode-fn)
  "Fontify TEXT using MODE-FN, return propertized string."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (funcall mode-fn))
    (font-lock-ensure)
    (buffer-string)))

(defun mr-x/fragment-highlight--apply-faces (start fontified-text)
  "Apply face properties from FONTIFIED-TEXT as overlays starting at START."
  (let ((pos 0)
        (len (length fontified-text)))
    (while (< pos len)
      (let ((face (get-text-property pos 'face fontified-text))
            (next-change (or (next-single-property-change pos 'face fontified-text) len)))
        (when face
          (let ((ov (make-overlay (+ start pos) (+ start next-change))))
            (overlay-put ov 'face face)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'category 'mr-x-fragment-highlight)))
        (setq pos next-change)))))

(defun mr-x/fragment-highlight--process-region (start end)
  "Find and highlight code blocks between START and END."
  (save-excursion
    (goto-char start)
    (let ((code-block-re (rx bol (* space) "```" (* space)
                             (group (* (any alnum "-" "+" "#")))
                             (* space) "\n"
                             (group (*? anything))
                             "\n" (* space) "```" (* space) (or "\n" eol))))
      (while (re-search-forward code-block-re end t)
        (let* ((lang (match-string 1))
               (body (match-string 2))
               (body-start (match-beginning 2))
               (mode-fn (mr-x/fragment-highlight--resolve-mode lang)))
          (when (and mode-fn (fboundp mode-fn) (> (length body) 0))
            (condition-case err
                (let ((fontified (mr-x/fragment-highlight--fontify-string body mode-fn)))
                  (mr-x/fragment-highlight--apply-faces body-start fontified)
                  (message "Highlighted %s block (%d chars)" lang (length body)))
              (error
               (message "Failed to highlight %s: %s" lang (error-message-string err))))))))))

(defun mr-x/fragment-highlight--clear ()
  "Remove all fragment highlighting overlays."
  (remove-overlays (point-min) (point-max) 'category 'mr-x-fragment-highlight))

;;; ============================================================================
;;; Interactive Commands
;;; ============================================================================

(defun mr-x/fragment-test-highlight ()
  "Apply syntax highlighting to all code blocks in test buffer."
  (interactive)
  (mr-x/fragment-highlight--clear)
  (let ((count 0))
    (dolist (frag mr-x/fragment-test-fragments)
      (let ((body-start (plist-get frag :body-start))
            (body-end (plist-get frag :body-end))
            (collapsed (plist-get frag :collapsed)))
        (unless collapsed
          (mr-x/fragment-highlight--process-region body-start body-end)
          (cl-incf count))))
    (message "Highlighted %d fragments" count)))

(defun mr-x/fragment-test-reset ()
  "Remove all highlighting and re-render fragments."
  (interactive)
  (mr-x/fragment-highlight--clear)
  (message "Cleared all fragment highlights"))

(defun mr-x/fragment-test-toggle ()
  "Toggle the fragment at point."
  (interactive)
  (let* ((pos (point))
         (frag-idx (get-text-property pos 'mr-x-fragment-id)))
    (unless frag-idx
      ;; Search backwards for fragment indicator
      (save-excursion
        (while (and (not frag-idx) (> (point) (point-min)))
          (backward-char)
          (setq frag-idx (get-text-property (point) 'mr-x-fragment-id)))))
    (if frag-idx
        (let* ((frag (nth frag-idx mr-x/fragment-test-fragments))
               (body-start (plist-get frag :body-start))
               (body-end (plist-get frag :body-end))
               (collapsed (plist-get frag :collapsed))
               (inhibit-read-only t))
          ;; Toggle visibility
          (put-text-property body-start body-end 'invisible (not collapsed))
          ;; Update indicator
          (save-excursion
            (goto-char (plist-get frag :start))
            (delete-char 2)
            (insert (propertize (if collapsed "▼ " "▶ ")
                                'face '(:foreground "#fabd2f")
                                'mr-x-fragment-indicator t
                                'mr-x-fragment-id frag-idx)))
          ;; Update state
          (plist-put frag :collapsed (not collapsed))
          (message "Fragment %s" (if collapsed "expanded" "collapsed")))
      (message "No fragment at point"))))

(defun mr-x/fragment-test-next ()
  "Move to next fragment."
  (interactive)
  (let ((found nil))
    (save-excursion
      (forward-char)
      (while (and (not found) (< (point) (point-max)))
        (when (get-text-property (point) 'mr-x-fragment-indicator)
          (setq found (point)))
        (forward-char)))
    (if found
        (goto-char found)
      (message "No more fragments"))))

(defun mr-x/fragment-test-prev ()
  "Move to previous fragment."
  (interactive)
  (let ((found nil))
    (save-excursion
      (backward-char)
      (while (and (not found) (> (point) (point-min)))
        (when (get-text-property (point) 'mr-x-fragment-indicator)
          (setq found (point)))
        (backward-char)))
    (if found
        (goto-char found)
      (message "No more fragments"))))

;;; ============================================================================
;;; Test Mode
;;; ============================================================================

(defvar mr-x/fragment-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h") #'mr-x/fragment-test-highlight)
    (define-key map (kbd "C-c r") #'mr-x/fragment-test-reset)
    (define-key map (kbd "C-c t") #'mr-x/fragment-test-toggle)
    (define-key map (kbd "TAB") #'mr-x/fragment-test-toggle)
    (define-key map (kbd "C-c n") #'mr-x/fragment-test-next)
    (define-key map (kbd "C-c p") #'mr-x/fragment-test-prev)
    (define-key map (kbd "n") #'mr-x/fragment-test-next)
    (define-key map (kbd "p") #'mr-x/fragment-test-prev)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for fragment test mode.")

(define-derived-mode mr-x/fragment-test-mode special-mode "FragTest"
  "Major mode for testing fragment syntax highlighting."
  (setq buffer-read-only nil)
  (setq truncate-lines t))

;;;###autoload
(defun mr-x/fragment-test-harness ()
  "Open the fragment highlight test harness."
  (interactive)
  (let ((buf (get-buffer-create "*Fragment Highlight Test*")))
    (with-current-buffer buf
      (mr-x/fragment-test-mode)
      (mr-x/fragment-test--render-all)
      (setq buffer-read-only t))
    (pop-to-buffer buf)
    (message "Fragment test harness ready. Press C-c h to apply highlighting.")))

(provide 'fragment-highlight-test)

;;; fragment-highlight-test.el ends here
