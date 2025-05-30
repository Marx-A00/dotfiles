;;; -*- lexical-binding: t; -*-

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :depth 1
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
						  ,@(when-let* ((depth (plist-get order :depth)))
						      (list (format "--depth=%d" depth) "--no-single-branch"))
						  ,(plist-get order :repo) ,repo))))
		  ((zerop (call-process "git" nil buffer t "checkout"
					(or (plist-get order :ref) "--"))))
		  (emacs (concat invocation-directory invocation-name))
		  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
					"--eval" "(byte-recompile-directory \".\" 0 'force)")))
		  ((require 'elpaca))
		  ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Disable package.el
(setq package-enable-at-startup nil)

(elpaca elpaca-use-package
  ;; Enable Elpaca support for use-package's :ensure keyword.
  (elpaca-use-package-mode))

(pcase system-type
  ('gnu/linux "It's Linux!")
  ('windows-nt "It's Windows!")
  ('darwin "It's macOS!"))

(if (daemonp)
    (message "Loading in the daemon!")
  (message "Loading in regular Emacs!"))

(defun mr-x/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "Iosevka" :height 280))

  ;; Set the fixed pitch face

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (mr-x/set-font-faces))))
  (mr-x/set-font-faces))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package no-littering
  :ensure t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror)
  (no-littering-theme-backups))

(use-package vterm
  :ensure t)

  (use-package multi-vterm
      :ensure t
      :after evil
      :config

      (add-hook 'vterm-mode-hook
		      (lambda ()
		      (setq-local evil-insert-state-cursor 'box)
		      (evil-insert-state)))
      (define-key vterm-mode-map [return]                      #'vterm-send-return)

      (setq vterm-keymap-exceptions nil)
      ;; dedicated terminal height of 30%
      (setq multi-vterm-dedicated-window-height-percent 40)
      (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
      (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
      (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
      (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
      (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
      (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
      (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
      (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))


  ;; Optional: set the shell explicitly if needed
  ;; (setq vterm-shell "/bin/zsh")


;; (use-package multi-vterm
;; 	 :config
;; 	 (add-hook 'vterm-mode-hook
;; 			 (lambda ()
;; 			 (setq-local evil-insert-state-cursor 'box)
;; 			 (evil-insert-state)))

;; 	 (define-key vterm-mode-map [return]                      #'vterm-send-return)

;; 	 (setq vterm-keymap-exceptions nil))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-sourcerer))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  (setq doom-modeline-modal-modern-icon nil))


(set-face-attribute 'default nil :font "Iosevka" :height 280)

(defun mr-x/general-setup ()
  (display-line-numbers-mode 1)
  (set-frame-parameter (selected-frame) 'alpha '(80 50)))

(add-hook 'text-mode-hook #'mr-x/general-setup)
(add-hook 'prog-mode-hook #'mr-x/general-setup)

					; opacity
(set-frame-parameter (selected-frame) 'alpha '(80 50))
(add-to-list 'default-frame-alist '(alpha-background . 20))
					; keybindings section
(global-set-key (kbd "C-<escape>") #'universal-argument)
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts
(global-set-key (kbd "C-c l") #'org-store-link) ; Suggested Key-binding from org-manual
(global-set-key (kbd "C-c a") #'org-agenda) ; Suggested Key-binding from org-manual
(global-set-key (kbd "C-c c") #'org-capture) ; Suggested Key-binding from org-manual



(setq inhibit-startup-message t) ; Disable the startup message
(scroll-bar-mode -1) ; Disable the visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(menu-bar-mode -1)   ; Disable the menu bar
(set-fringe-mode 10) ; Give some breathing room

(use-package perspective
:ensure t
:bind
("C-x C-b" . persp-counsel-switch-buffer)         ; or use a nicer switcher, see below
("C-x C-i" . persp-ibuffer)
:custom
(persp-mode-prefix-key (kbd "C-x M-x"))  ; pick your own prefix key here
:init
(persp-mode))

(defun mr-x/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'org-mode-hook #'mr-x/org-mode-visual-fill))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package highlight
  :ensure t)

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "\
# Clear your mind young one.")

(use-package general
      :ensure t
      :demand t
      :config
      ;; allow for shorter bindings -- e.g., just using things like nmap alone without general-* prefix
      (general-evil-setup t)

      ;; To automatically prevent Key sequence starts with a non-prefix key errors without the need to
      ;; explicitly unbind non-prefix keys, you can add (general-auto-unbind-keys) to your configuration
      ;; file. This will advise define-key to unbind any bound subsequence of the KEY. Currently, this
      ;; will only have an effect for general.el key definers. The advice can later be removed with
      ;; (general-auto-unbind-keys t).
      (general-auto-unbind-keys))

      (with-eval-after-load 'general
	(general-create-definer mr-x/leader-def
	  :states '(normal visual motion emacs insert)
	  :keymaps 'override
	  :prefix "SPC"
	  :global-prefix "C-SPC"))

      (with-eval-after-load 'general
	(mr-x/leader-def
	  "a" 'mr-x/org-agenda-custom
	  ;; "m" 'mu4e
	  "f" 'link-hint-open-link
	  ;; "p" 'projectile-command-map
	  "h" 'winner-undo
	  "l" 'winner-redo
	  ;; "s" 'mr-x/toggle-shortcuts
	  ;; "S" 'mr-x/scratch
	  ;; "v" 'multi-vterm
	  "e" '(lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/emacs/.emacs.d/emacs.org")))
	  "1" (lambda () (interactive) (persp-switch-by-number 1))
	  "2" (lambda () (interactive) (persp-switch-by-number 2))
	  "3" (lambda () (interactive) (persp-switch-by-number 3))
	  "4" (lambda () (interactive) (persp-switch-by-number 4))
	  "5" (lambda () (interactive) (persp-switch-by-number 5)))

	(mr-x/leader-def
	  "d" '(:ignore t :wk "Dired")
	  "d d" '(dired :wk "Open Dired")
	  "d j" '(dired-jump :wk "Dired jump to current")
	  "d H" '(dired-omit-mode :wk "Dired Omit Mode"))

	(mr-x/leader-def
	"b" '(:ignore t :wk "buffer")
	"b b" '(persp-counsel-switch-buffer :wk "switch buffer")
	"b k" '(kill-this-buffer :wk "kill this buffer")
	"b r" '(revert-buffer :wk "revert buffer"))
	
	(mr-x/leader-def
	"v" '(:ignore t :wk "vterm")
	"v v" '(multi-vterm :wk "multi-vterm")
	"v n" '(multi-vterm-next :wk "multi-vterm-next")
	"v p" '(multi-vterm-prev :wk "multi-vterm-prev")
	"v d" '(multi-vterm-dedicated-toggle :wk "multi-vterm-dedicated-toggle")))

;; add multi-vterm-project in projectile prolly


	(defun mr-x/org-agenda-day ()
	  (interactive)
	  (org-agenda nil "a"))

	(defun mr-x/org-agenda-custom ()
	  (interactive)
	  (org-agenda nil "c"))

(winner-mode 1)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

(use-package link-hint
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-separator " → ")
  (setq which-key-idle-delay 1))

(use-package evil
  :ensure t
  :demand t
  :init (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-collection
    :ensure t
    :after (evil ivy)
    :config
    (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package dired
  :ensure nil  
  :commands (dired dired-jump)
  :config
  (setq insert-directory-program "gls")
  (setq dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first")
  (evil-define-key 'normal dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)

  (add-hook 'dired-mode-hook
	(lambda ()
	  (dired-omit-mode 1)
	  (dired-hide-details-mode 1))))

(use-package dired-x
  :ensure nil 
  :after dired
  :config
  (setq dired-omit-files (rx (seq bol "."))))


  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode))

  (setq display-line-numbers-type 'relative)
  (dolist (mode '(text-mode-hook prog-mode-hook conf-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Ivy & Counsel

(use-package swiper
  :ensure t)

(use-package ivy
  :ensure t
  :bind (("C-s" . swiper)
	   :map ivy-minibuffer-map
	   ("TAB" . ivy-alt-done)
	   ("C-l" . ivy-alt-done)
	   ("C-j" . ivy-next-line)
	   ("C-k" . ivy-previous-line)
	   :map ivy-switch-buffer-map
	   ("C-k" . ivy-previous-line)
	   ("C-l" . ivy-done)
	   ("C-d" . ivy-switch-buffer-kill)
	   :map ivy-reverse-i-search-map
	   ("C-k" . ivy-previous-line)
	   ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) "))

;; Taken from emacswiki to search for symbol/word at point
;; Must be done at end of init I guess
;; (define-key swiper-map (kbd "C-.")
;; 	    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))

;; (define-key swiper-map (kbd "M-.")
;; 	    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))


(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; org (kinda not really)

    (use-package toc-org
      :ensure t
      :commands toc-org-enable
      :hook (org-mode . toc-org-mode))

    (defun mr-x/org-mode-setup()

      (visual-line-mode 1)
      (auto-fill-mode 0)
	    (setq org-hide-leading-stars t)
      (setq org-agenda-include-diary t)
      (setq org-fold-core-style 'overlays)
      (setq org-agenda-span 'day)
      (setq evil-auto-indent nil))

    (setq org-agenda-files
	  '("~/roaming/agenda.org"
	    "~/roaming/habits.org"
	    "~/jira"))
    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)

    (use-package org
      :hook (org-mode . mr-x/org-mode-setup)
      :config
      (setq org-hide-emphasis-markers t)
      (setq org-agenda-start-with-log-mode t)
      (setq org-log-done 'time)
      (setq org-log-into-drawer t)

      ;; testing

      (setq org-M-RET-may-split-line '((default . nil)))
      (setq org-list-automatic-rules 
	    '((checkbox . t)
	     (indent . nil)
	     (ordered . nil)))

      ;; doesn't work lol thanks oai

    ;;   (defun my/org-meta-return-auto-checkbox (&rest _)
    ;; "Extend `M-RET` to insert a checkbox automatically."
    ;; (when (org-at-item-checkbox-p)
    ;;   (insert "[ ] ")))

    ;;   (advice-add 'org-meta-return :after #'my/org-meta-return-auto-checkbox)




      (setq org-highlight-latex-and-related '(latex))

					    ; org- habit setup

      (require 'org-habit)
      (add-to-list 'org-modules 'org-habit)
      (setq org-habit-graph-column 60)

      (setq org-todo-keywords
	    '((sequence
	       "TODO(t)"
	       "NEXT(n)"
	       "|"
	       "DONE(d!)")
	      (sequence
	       "BACKLOG(b)"
	       "PLAN(p)"
	       "READY(r)"
	       "IN-PROGRESS(i)"
	       "REVIEW(v)"
	       "WATCHING(w@/!)"
	       "HOLD(h)"
	       "|"
	       "COMPLETED(c)"
	       "CANC(k@)")))

      (setq org-todo-keyword-faces
	    '(("TODO" . "#FF1800")
	      ("NEXT" . "#FF1800")
	      ("PLAN" . "#F67F2F")
	      ("DONE" . "#62656A")
	      ("HOLD" . "#62656A")
	      ("WAIT" . "#B7CBA8")
	      ("IN-PROGRESS" . "#b7cba8") 
	      ("BACKLOG" . "#62656A")))

      (custom-set-faces
       '(org-level-1 ((t (:foreground "#ff743f")))))

      (custom-set-faces
       '(org-level-2 ((t (:foreground "#67bc44")))))

      (custom-set-faces
       '(org-level-3 ((t (:foreground "#67c0de"))))))

    (use-package org-superstar
      :ensure t
      :hook (org-mode . org-superstar-mode)
      :config
      (setq org-superstar-headline-bullets-list
	    '("🃏" "⡂" "⡆" "⢴" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
      (setq org-ellipsis " ‧"))


    ;; org agenda
    (setq org-agenda-skip-scheduled-if-done t
	  org-agenda-skip-deadline-if-done t
	  org-agenda-include-deadlines t
	  org-agenda-block-separator #x2501
	  org-agenda-compact-blocks t
	  org-agenda-start-with-log-mode t)

    (setq org-agenda-clockreport-parameter-plist
	  (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
    (setq org-agenda-deadline-faces
	  '((1.0001 . org-warning)              ; due yesterday or before
	    (0.0    . org-upcoming-deadline)))  ; due today or later

    (defun org-habit-streak-count ()
      (goto-char (point-min))
      (while (not (eobp))
	;;on habit line?
	(when (get-text-property (point) 'org-habit-p)
	  (let ((streak 0)
		(counter (+ org-habit-graph-column (- org-habit-preceding-days org-habit-following-days)))
		)
	    (move-to-column counter)
	    ;;until end of line
	    (while (= (char-after (point)) org-habit-completed-glyph)
	      (setq streak (+ streak 1))
	      (setq counter (- counter 1))
	      (backward-char 1))
	    (end-of-line)
	    (insert (number-to-string streak))))
	(forward-line 1)))

    (add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)

    (defun my/style-org-agenda()
      (setq org-agenda-window-setup 'only-window)
      (set-face-attribute 'org-agenda-date nil :height 1.1)
      (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
      (set-face-attribute 'org-agenda-date-today nil
			  :foreground "#897d6c"   
			  :background nil        
			  :weight 'bold
			  :underline nil)           ;; Make it bold
      (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))

    (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)



    (setq org-agenda-breadcrumbs-separator " ❱ "
	  org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
	  org-agenda-time-grid '((daily today)
				 (800 1000 1200 1400 1600 1800 2000)
				 "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
	  org-agenda-prefix-format '((agenda . "%i %-12:c [%e] %?-12t%b% s")
				     (todo . " %i %-12:c [%e] ")
				     (tags . " %i %-12:c")
				     (search . " %i %-12:c")))




    (setq org-agenda-custom-commands
	  '(("p" "Projects Agenda"
	     ((todo "NEXT"
		    ((org-agenda-overriding-header
		      (concat "Projects\n" (make-string (window-width) 9472) "\n\n"))
		     (org-agenda-files '("~/roaming/notes/20250211154648-stable_elpaca.org"
					 "~/roaming/notes/20250212103431-customize_org_agenda.org"
					 "~/roaming/notes/20240507202146-openpair.org"
					 "~/roaming/notes/20250107142334-rec.org"
					 "~/roaming/notes/20250210175701-amazon_orders_sorting.org"
					 "~/roaming/notes/20250220152855-personal_website.org"
					 "~/roaming/notes/20240708090814-guitar_fretboard_js.org"
					 "~/roaming/notes/20240416191540-typingpracticeapplication.org"))))))
	    ("c" "Custom Projects & Agenda"
	     ((agenda ""
		      ((org-agenda-overriding-header "Agenda")
		       (org-agenda-prefix-format
			'((agenda . "  %?-12t% s")
			  (timeline . "  % s")
			  (todo . "  ")
			  (tags . "  ")
			  (search . "  ")))
		       (org-agenda-log-mode-items '(closed clock))))
	      (todo "NEXT"
		    ((org-agenda-overriding-header
		      (concat "\nProjects\n" (make-string (window-width) 9472) "\n"))
		     (org-agenda-files '("~/roaming/notes/20250211154648-stable_elpaca.org"
					 "~/roaming/notes/20250212103431-customize_org_agenda.org"
					 "~/roaming/notes/20240507202146-openpair.org"
					 "~/roaming/notes/20250107142334-rec.org"
					 "~/roaming/notes/20250210175701-amazon_orders_sorting.org"
					 "~/roaming/notes/20250220152855-personal_website.org"
"~/roaming/notes/20250317082044-vibe_coding_video.org"
"~/roaming/notes/20250402103112-kountdown.org"
					 "~/roaming/notes/20240708090814-guitar_fretboard_js.org"
					 "~/roaming/notes/20250309222443-virtual_museum.org"
					 "~/roaming/notes/20250402092144-track01_s_w.org"
					 "~/roaming/notes/20240416191540-typingpracticeapplication.org")))))
	     nil)))
    (setq org-agenda-format-date (lambda (date)
				   (concat"\n"(make-string(window-width)9472)
					  "\n"(org-agenda-format-date-aligned date))))
    (setq org-cycle-separator-lines 2)

    (add-hook 'org-agenda-finalize-hook
	      (lambda ()
		(setq visual-fill-column-width 100) 
		(setq visual-fill-column-center-text t)
		(visual-fill-column-mode t)
		(display-line-numbers-mode 1)))






(defun my-highlight-lowest-goal ()
  "Find and highlight the task in the 'Projects' section with the lowest 'GOAL #' number."
  (when (derived-mode-p 'org-agenda-mode)
    (save-excursion
      (goto-char (point-min))
      (let (lowest-goal lowest-pos)
	;; Search for "Projects" section
	(when (re-search-forward "^Projects" nil t)
	  ;; Iterate over tasks under "Projects"
	  (while (re-search-forward "GOAL #\\([0-9]+\\)" nil t)
	    (let* ((goal-num (string-to-number (match-string 1)))
		   (line-start (line-beginning-position))
		   (line-end (line-end-position)))
	      ;; Track the lowest goal number and its position
	      (when (or (not lowest-goal) (< goal-num lowest-goal))
		(setq lowest-goal goal-num)
		(setq lowest-pos (cons line-start line-end))))))
	;; Apply highlighting to the first occurrence of the lowest goal
	(when lowest-pos
	  (let ((ov (make-overlay (car lowest-pos) (cdr lowest-pos))))
	    (overlay-put ov 'face '(:background "dark red" :foreground "white" :weight bold))))))))


(add-hook 'org-agenda-finalize-hook #'my-highlight-lowest-goal)

(use-package ob-typescript
    :ensure t
    (:wait t))

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
	   (js . t)
	   (typescript . t)
	   (sqlite . t)
	   (sql . t)
	   (latex . t)
	   (python . t)))

	   (setq org-babel-python-command "python3")
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("C" . "comment"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("l" . "export latex"))

   ;; Automatically tangle our Emacs.org config file when we save it
   (defun mr-x/org-babel-tangle-config ()
     (when (string-equal (buffer-file-name)
			  (expand-file-name "~/.dotfiles/emacs/.emacs.d/emacs.org"))
       ;; Dynamic scoping to the rescue
       (let ((org-confirm-babel-evaluate nil))
	  (org-babel-tangle))))

   (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'mr-x/org-babel-tangle-config)))

   (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
					 ("#+END_SRC" . "†")
					 ("#+begin_src" . "†")
					 ("#+end_src" . "†")
					 ("#+BEGIN_LaTeX" . "†")
					 ("#+END_LaTeX" . "†")
					 (">=" . "≥")
					 ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(use-package org-roam
   :ensure t
   :demand t
   :custom
   (org-roam-directory "~/roaming/notes/")
   (org-roam-completion-everywhere t)
   ;; (org-roam-capture-templates
   ;;  '(("d" "default" plain
   ;; 	"%?"
   ;; 	:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n+date: %U\n")
   ;; 	:unnarrowed t)
   ;;    ("w" "workout" plain
   ;; 	"%?"
   ;; 	:if-new (file+head "workouts/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   ;; 	:unnarrowed t)
   ;;    ("l" "programming language" plain
   ;; 	"* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
   ;; 	:if-new (file+head "code-notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   ;; 	:unnarrowed t)
   ;;    ("b" "book notes" plain
   ;; 	(file "~/roaming/Templates/BookNoteTemplate.org")
   ;; 	:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   ;; 	:unnarrowed t)
   ;;    ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
   ;; 	:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
   ;; 	:unnarrowed t)))
   ;; (org-roam-dailies-capture-templates
   ;;  '(("d" "default" entry "* %<%I:%M %p>: %?"
   ;; 	:if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

   :bind (("C-c n f" . org-roam-node-find)
	   ("C-c n i" . org-roam-node-insert)
	   ("C-c n I" . org-roam-node-insert-immediate)
					  ; ("C-c n p" . my/org-roam-find-project)
					  ;("C-c n t" . my/org-roam-capture-task)
					  ; ("C-c n b" . my/org-roam-capture-inbox)
	   :map org-mode-map
	   ("C-M-i"   . completion-at-point)
	   :map org-roam-dailies-map
	   ("Y" . org-roam-dailies-capture-yesterday)
	   ("T" . org-roam-dailies-capture-tomorrow))
   :bind-keymap
   ("C-c n d" . org-roam-dailies-map)
   :config
   (require 'org-roam-dailies)

   (org-roam-db-autosync-mode))
(setq org-roam-dailies-directory "journal/")


 ;; Bind this to C-c n I
 (defun org-roam-node-insert-immediate (arg &rest args)
   (interactive "P")
   (let ((args (cons arg args))
	  (org-roam-capture-templates (list (append (car org-roam-capture-templates)
						    '(:immediate-finish t)))))
     (apply #'org-roam-node-insert args)))

(with-eval-after-load 'org-roam
  (require 'org-roam-node)
 (defun my/org-roam-filter-by-tag (tag-name)
   (lambda (node)
     (member tag-name (org-roam-node-tags node))))

 (defun my/org-roam-list-notes-by-tag (tag-name)
   (mapcar #'org-roam-node-file
	    (seq-filter
	     (my/org-roam-filter-by-tag tag-name)
	     (org-roam-node-list))))

 (defun my/org-roam-refresh-agenda-list ()
   (interactive)
   (setq org-agenda-files
	 (append
	  (my/org-roam-list-notes-by-tag "Project")
	  (directory-files-recursively
	   (expand-file-name org-roam-dailies-directory org-roam-directory)
	   "\\.org$"))))

 (my/org-roam-refresh-agenda-list))

 (defun my/org-roam-project-finalize-hook ()
   "Adds the captured project file to `org-agenda-files' if the
	   capture was not aborted."
   ;; Remove the hook since it was added temporarily
   (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

   ;; Add project file to the agenda list if the capture was confirmed
   (unless org-note-abort
     (with-current-buffer (org-capture-get :buffer)
	(add-to-list 'org-agenda-files (buffer-file-name)))))


 (defun my/org-roam-find-project ()
   (interactive)
   ;; Add the project file to the agenda after capture is finished
   (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

   ;; Select a project file to open, creating it if necessary
   (org-roam-node-find
    nil
    nil
    (my/org-roam-filter-by-tag "Project")
    nil
    :templates
    '(("p" "project" plain
	"* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
	:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
	:unnarrowed t))))

 (global-set-key (kbd "C-c n p") #'my/org-roam-find-project)


 (defun my/org-roam-capture-inbox ()
   (interactive)
   (org-roam-capture- :node (org-roam-node-create)
		       :templates '(("i" "inbox" plain "* %?"
				     :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

 (global-set-key (kbd "C-c n b") #'my/org-roam-capture-inbox)


 (defun my/org-roam-capture-task ()
   (interactive)
   ;; Add the project file to the agenda after capture is finished
   (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

   ;; Capture the new task, creating the project file if necessary
   (org-roam-capture- :node (org-roam-node-read
			      nil
			      (my/org-roam-filter-by-tag "Project"))
		       :templates '(("p" "project" plain "** TODO %?"
				     :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
							    "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
							    ("Tasks"))))))

 (global-set-key (kbd "C-c n t") #'my/org-roam-capture-task)



 (defun my/org-roam-copy-todo-to-today ()
   (interactive)
   (let ((org-refile-keep t) ;; Set this to nil to delete the original!
	  (org-roam-dailies-capture-templates
	   '(("t" "tasks" entry "%?"
	      :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
	  (org-after-refile-insert-hook #'save-buffer)
	  today-file
	  pos)

     ;; Check if the task is a habit by checking the STYLE property
     (unless (string= (org-entry-get nil "STYLE") "habit")
	(save-window-excursion
	  (org-roam-dailies--capture (current-time) t)
	  (setq today-file (buffer-file-name))
	  (setq pos (point)))

	;; Only refile if the target file is different than the current file
	(unless (equal (file-truename today-file)
		       (file-truename (buffer-file-name)))
	  (org-refile nil nil (list "Tasks" today-file nil pos))))))



 (add-to-list 'org-after-todo-state-change-hook
	       (lambda ()
		 (when (or (equal org-state "DONE")
			   (equal org-state "CANC"))
		   (my/org-roam-copy-todo-to-today))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
  org-roam-ui-follow t
  org-roam-ui-update-on-save t
  org-roam-ui-open-on-start t))

(use-package ox-hugo
  :ensure t
  :after (ox))

(use-package simple-httpd
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-indent-mode -1)

(use-package typescript-mode
      :ensure t
      :mode "\\.ts\\'"
      :config
      (setq typescript-indent-level 2))

    (use-package web-mode
      :ensure t
      :config
      (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(use-package ledger-mode
  :ensure t
  :mode ("\\.dat\\'"
	 "\\.ledger\\'")
  :bind (:map ledger-mode-map
	      ("C-x C-s" . my/ledger-save))
  :preface
  (defun my/ledger-save ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (save-excursion
      (when (buffer-modified-p)
	(with-demoted-errors (ledger-mode-clean-buffer))
	(save-buffer)))))
