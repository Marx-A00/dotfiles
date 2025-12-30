;;; -*- lexical-binding: t; -*-

(defvar elpaca-installer-version 0.11)
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

(elpaca elpaca-use-package
  ;; Enable Elpaca support for use-package's :ensure keyword.
  (elpaca-use-package-mode))

;; Load org early to avoid version conflicts
(elpaca org)
(elpaca-wait)

;; Ensure .org files always open in org-mode (fixes version mismatch issues)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Function to fix any .org files stuck in fundamental-mode
(defun mr-x/fix-org-mode-buffers ()
  "Fix any .org files that opened in fundamental-mode"
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)
               (string-match "\\.org$" (buffer-file-name buffer))
               (not (eq (with-current-buffer buffer major-mode) 'org-mode)))
      (with-current-buffer buffer
        (org-mode)))))

(use-package org
      :ensure nil
      :demand t
	:hook (org-mode . mr-x/org-mode-setup)
	:config
	;; Move org-agenda-files here
	(setq org-agenda-files '("~/roaming/agenda.org"))
	
	;; Move clock persistence here  
	(setq org-clock-persist t)
	(org-clock-persistence-insinuate)
	
	;; Define setup function here
	(defun mr-x/org-mode-setup()
	  (visual-line-mode 1)
	  (auto-fill-mode 0)
	  (setq org-hide-leading-stars t)
	  (setq org-agenda-include-diary t)
	  (setq org-fold-core-style 'overlays)
	  (setq org-agenda-span 'day)
	  (setq evil-auto-indent nil))
	
	;; Rest of org config
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
					   "~/roaming/notes/20250701091830-omi_live.org"

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
					   "~/roaming/notes/20250701091830-omi_live.org"
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

;; toc-org for table of contents generation
(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

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
   (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

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

(use-package transient
  :ensure t
  :demand t
  :init
  ;; Remove built-in transient from load-path to force using the newer version
  (setq load-path (delete (expand-file-name "transient" (locate-library "transient")) load-path)))

(pcase system-type
  ('gnu/linux "It's Linux!")
  ('windows-nt "It's Windows!")
  ('darwin "It's macOS!"))

(if (daemonp)
    (message "Loading in the daemon!")
  (message "Loading in regular Emacs!"))

(defun mr-x/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "JuliaMono" :height 240))

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
  ;; Run on macOS or when running as daemon (window-system is nil in daemon)
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(use-package no-littering
  :ensure t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror)
  (no-littering-theme-backups))

(use-package vterm
  :ensure t
  :config
  ;; Prevent evil-collection from messing with vterm keys
  (with-eval-after-load 'evil-collection
    (setq evil-collection-vterm-bind-escape-in-insert nil)
    (delete 'vterm evil-collection-mode-list)))

(use-package multi-vterm
  :ensure t
  :after (evil vterm)
  :config
  ;; Set buffer name prefix to "vterm"
  (setq multi-vterm-buffer-name "vterm")
  ;; Set dedicated terminal height
  (setq multi-vterm-dedicated-window-height-percent 40)
  
  ;; Setup vterm with evil properly
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))
  
  ;; Basic vterm configuration
  (setq vterm-keymap-exceptions nil)
  
  ;; Define keys after vterm-mode-map exists
  (with-eval-after-load 'vterm
    (define-key vterm-mode-map [return] #'vterm-send-return)
    
    ;; Pass through common terminal keys in insert mode
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
    
    ;; Normal mode keys
    (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
    (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
    (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
    (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
    (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
    (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)))




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
    (load-theme 'doom-gruvbox))

  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom
    ;; Make modeline smaller and cleaner
    (doom-modeline-height 25)  ; Smaller height (default is 25)
    (doom-modeline-bar-width 3)  ; Thinner bar
    
    ;; Remove encoding to save space
    (doom-modeline-buffer-encoding nil)  ; Remove encoding indicator
    
    ;; Other space-saving options
    (doom-modeline-indent-info nil)  ; Remove indentation info
    (doom-modeline-minor-modes nil)  ; Hide minor modes (they take lots of space)
    (doom-modeline-buffer-file-name-style 'truncate-upto-root)  ; Shorter file paths
    
    ;; Keep useful stuff
    (doom-modeline-major-mode-icon t)  ; Keep major mode icon
    (doom-modeline-major-mode-color-icon t)  ; Colorful icons
    (doom-modeline-buffer-state-icon t)  ; Modified/saved state
    (doom-modeline-buffer-modification-icon t)  ; Show if modified
    (doom-modeline-lsp t)  ; Keep LSP indicator
    (doom-modeline-github nil)  ; Remove github notifications (saves space)
    (doom-modeline-persp-name nil)  ; Remove perspective name if not using
    (doom-modeline-modal-modern-icon nil)  ; Your existing setting
    
    ;; Performance
    (doom-modeline-checker-simple-format t)  ; Simpler error format
    (doom-modeline-env-version nil)  ; Don't show environment version
    (doom-modeline-unicode-fallback t))  ; Use unicode when icons unavailable
    
    :config
    ;; Commented out to prevent modeline shrinking when frame loses focus
    ;; (set-face-attribute 'mode-line nil :height 0.9)
    ;; (set-face-attribute 'mode-line-inactive nil :height 0.9))


  ;; (set-face-attribute 'default nil :font "JuliaMono" :height 280)

  (defun mr-x/general-setup ()
    (display-line-numbers-mode 1)
    (set-frame-parameter (selected-frame) 'alpha '(80 50)))

  (add-hook 'text-mode-hook #'mr-x/general-setup)
  (add-hook 'prog-mode-hook #'mr-x/general-setup)

					  ; opacity
  (set-frame-parameter (selected-frame) 'alpha '(100 50))
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

;; Disable native fullscreen behavior
(setq ns-use-native-fullscreen nil)

;; Make new frames tile properly instead of floating
(setq ns-pop-up-frames nil)

;; Prevent Emacs from resizing frames
(setq frame-resize-pixelwise t)

(use-package perspective
:ensure t
:bind
("C-x C-b" . persp-counsel-switch-buffer)         ; or use a nicer switcher, see below
("C-x C-i" . persp-ibuffer)
:custom
(persp-mode-prefix-key (kbd "C-x M-x"))  ; keep original prefix for compatibility
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

;; Auto-revert mode to automatically refresh buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(use-package highlight
  :ensure t)

;; Popper for popup buffer management
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Compile-Log\\*"
          "\\*Backtrace\\*"
          "\\*evil-registers\\*"
          "\\*Apropos\\*"
          "\\*scratch\\*"
          "\\*Help\\*"
          "\\*helpful.*\\*"
          "\\*vterm.*\\*"
          "\\*compilation\\*"
          help-mode
          helpful-mode
          compilation-mode
          vterm-mode))
  :config
  (setq popper-display-control t)
  (setq popper-display-function #'popper-select-popup-at-bottom)
  (popper-mode +1)
  (popper-echo-mode +1))

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
      "p" 'projectile-command-map
      "w" '(:keymap evil-window-map :package evil :wk "window")
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
      "d h" '((lambda () (interactive) (dired "~/")) :wk "Dired home")
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
      "v d" '(multi-vterm-dedicated-toggle :wk "multi-vterm-dedicated-toggle"))

    (mr-x/leader-def
      "c" '(:ignore t :wk "Agent Shell")
      "c c" '(agent-shell :wk "Start Agent Shell")
      "c n" '(agent-shell-new-shell :wk "New shell")
      "c t" '(mr-x/agent-shell-toggle :wk "Toggle Agent Shell")
      "c w" '(mr-x/focus-ai-window :wk "Focus AI window")
      "c b" '(agent-shell-sidebar-toggle :wk "Toggle sidebar")
      "c B" '(agent-shell-manager-toggle :wk "Buffer manager")
      "c j" '(agent-shell-attention-jump :wk "Jump to pending")
      "c p" '(agent-shell-prompt-compose :wk "Compose prompt")
      "c r" '(agent-shell-send-region :wk "Send region")
      "c f" '(agent-shell-send-file :wk "Send file")
      "c F" '(agent-shell-send-other-file :wk "Send other file")
      "c d" '(agent-shell-send-dwim :wk "Send DWIM (region/error)")
      "c s" '(agent-shell-send-screenshot :wk "Send screenshot")
      "c i" '(agent-shell-interrupt :wk "Interrupt")
      "c o" '(agent-shell-other-buffer :wk "Other buffer (viewport/shell)")
      "c m" '(agent-shell-set-session-mode :wk "Set mode")
      "c M" '(agent-shell-cycle-session-mode :wk "Cycle mode")
      "c ." '(agent-shell-set-session-model :wk "Set model")
      "c T" '(agent-shell-open-transcript :wk "Open transcript")
      "c q" '(agent-shell-queue-request :wk "Queue request")
      "c l" '(:ignore t :wk "Logs")
      "c l l" '(agent-shell-toggle-logging :wk "Toggle logging")
      "c l v" '(agent-shell-view-traffic :wk "View traffic")
      "c l a" '(agent-shell-view-acp-logs :wk "View ACP logs")
      "c l r" '(agent-shell-reset-logs :wk "Reset logs"))




    (mr-x/leader-def
      "g" '(:ignore t :wk "git")
      "g g" '(magit-status :wk "magit status")
      "g d" '(magit-diff-unstaged :wk "diff unstaged")
      "g c" '(magit-branch-or-checkout :wk "branch or checkout")
      "g l" '(magit-log-current :wk "log current")
      "g L" '(magit-log-oneline :wk "log oneline")
      "g b" '(magit-blame :wk "blame")
      "g p" '(magit-push-current :wk "push current")
      "g P" '(magit-pull-branch :wk "pull branch")
      "g f" '(magit-fetch :wk "fetch"))

    ;; Bind after agent-shell loads
    (with-eval-after-load 'agent-shell
      (mr-x/leader-def
        "g m" '(mr-x/ai-commit-message :wk "AI commit message")))

    (mr-x/leader-def
      "x" '(:keymap perspective-map :wk "perspective"))


)

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
	    (dired-hide-details-mode 1)
	    (display-line-numbers-mode 1))))

(use-package dired-x
  :ensure nil 
  :after dired
  :config
  (setq dired-omit-files (rx (seq bol "."))))

(add-hook 'dired-mode-hook
	    (lambda () (setq-local dired-omit-verbose t)))
(setq dired-omit-verbose nil)



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

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  ;; Fix "Unknown terminal type" error when committing
  (setenv "TERM" "dumb")
  ;; Use with-editor for commit messages (Magit's built-in handling)
  (require 'with-editor))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  ;; Set the completion system to ivy since you're using it
  (setq projectile-completion-system 'ivy)
  ;; Configure project search paths
  (setq projectile-project-search-path '("~/roaming" "~/work"))
  ;; Set default action when switching projects
  (setq projectile-switch-project-action #'projectile-dired)
  ;; Use the hybrid indexing method for better performance
  (setq projectile-indexing-method 'hybrid)
  ;; Enable caching for better performance
  (setq projectile-enable-caching t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :ensure t
  :after (projectile counsel)
  :config 
  (counsel-projectile-mode 1))

(use-package ox-hugo
  :ensure t
  :after (ox))

(use-package simple-httpd
  :ensure t)

;; Remember last position in files (like a real bookmark)
(save-place-mode 1)


(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (evil-define-key 'normal pdf-view-mode-map (kbd "SPC") nil))  ;; compiles the epdfinfo server

;; Extend save-place to remember PDF page positions
(use-package saveplace-pdf-view
  :ensure t
  :after pdf-tools)

(use-package org-noter
  :ensure (:host github :repo "org-noter/org-noter")
  :after pdf-tools)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package markdown-xwidget
  :ensure (:host github
           :repo "cfclrk/markdown-xwidget"
           :files (:defaults "resources"))
  :after markdown-mode
  :bind (:map markdown-mode-map
         ("C-c C-c x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-github-theme "dark")
  (markdown-xwidget-code-block-theme "gruvbox-dark-medium")
  :config
  ;; Add custom CSS for larger text after preview mode enables
  (defvar mr-x/markdown-custom-css
    (expand-file-name "etc/markdown-custom.css" user-emacs-directory))
  (advice-add 'markdown-xwidget-preview-mode--enable :after
              (lambda ()
                (add-to-list 'markdown-css-paths mr-x/markdown-custom-css t))))

;; Automatically install and use tree-sitter grammars
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)  ;; Ask before installing grammars
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Corfu for in-buffer completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix
  (corfu-auto-delay 0.2)         ;; Delay before auto completion
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  :init
  (global-corfu-mode))

;; Extensions for Corfu
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; LSP Mode for Language Server Protocol support
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; Performance tuning
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  
  ;; UI settings
  (setq lsp-lens-enable nil)              ;; No code lens
  (setq lsp-headerline-breadcrumb-enable nil)  ;; No breadcrumbs
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-completion-provider :none)  ;; Let Corfu handle completion
  (setq lsp-enable-snippet t))

;; LSP UI for better LSP experience
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil)       ;; No sideline
  (setq lsp-ui-doc-enable t)              ;; Keep documentation
  (setq lsp-ui-doc-show-with-cursor nil)  ;; Don't show automatically
  (setq lsp-ui-doc-show-with-mouse t)     ;; Show on mouse hover
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-peek-enable t))

;; Optional: Kind icon for completion candidates
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-indent-mode -1)

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :hook ((emacs-lisp-mode . flycheck-mode)
         (emacs-lisp-mode . (lambda ()
                              ;; Disable checkdoc nagging about docstrings
                              (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Modern TypeScript setup with tree-sitter and LSP

;; Use built-in tree-sitter modes for TypeScript/TSX (Emacs 29+)
;; These provide better syntax highlighting and structural editing
(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-indent-level 2))

;; Add LSP support for TypeScript
(add-hook 'typescript-ts-mode-hook #'lsp-deferred)
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)
(add-hook 'js-ts-mode-hook #'lsp-deferred)

;; Configure LSP for TypeScript
(with-eval-after-load 'lsp-mode
  ;; TypeScript/JavaScript specific settings
  (setq lsp-typescript-preferences-import-module-specifier "relative"
        lsp-typescript-preferences-quote-style "single"
        lsp-typescript-format-enable t
        lsp-javascript-format-enable t
        lsp-typescript-tsserver-log-verbosity "off"
        lsp-javascript-display-return-type-hints t
        lsp-typescript-display-return-type-hints t))

;; Optional: Prettier formatting for TypeScript/JavaScript
(use-package prettier
  :ensure t
  :hook ((typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . prettier-mode)
         (js-ts-mode . prettier-mode)
         (json-ts-mode . prettier-mode)
         (css-mode . prettier-mode))
  :config
  (setq prettier-enabled-parsers '(typescript tsx javascript json css scss)))

;; Web-mode for legacy support and template files
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t))

;; Optional: Tailwind CSS LSP support
(use-package lsp-tailwindcss
  :ensure t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'tsx-ts-mode)
  (add-to-list 'lsp-tailwindcss-major-modes 'typescript-ts-mode))

(use-package monet
  :ensure (:host github :repo "https://github.com/stevemolitor/monet")
  :config
  ;; Use ediff as the diff tool
  (setq monet-diff-tool 'ediff)
  
  ;; Comment out Delta functions for now - will deal with later
  ;; ;; Custom Delta-powered diff function for beautiful syntax highlighting
  ;; (defun mr-x/monet-delta-diff (old-file-path new-file-path new-file-contents on-accept on-quit &optional session)
  ;;   "Display diff using Delta for beautiful syntax highlighting."
  ;;   ;; Write new content to the new file path
  ;;   (with-temp-file new-file-path
  ;;     (insert new-file-contents))
  ;;   
  ;;   ;; Just run the delta diff command and let Emacs handle the display
  ;;   (let ((cmd (format "git diff --no-index %s %s | delta --paging=never"
  ;;                      (shell-quote-argument old-file-path)
  ;;                      (shell-quote-argument new-file-path))))
  ;;     (async-shell-command cmd "*Monet Delta Diff*"))
  ;;   
  ;;   ;; Return simple context
  ;;   (list :buffer-name "*Monet Delta Diff*"))

  ;; 
  ;; ;; Simple cleanup function
  ;; (defun mr-x/monet-delta-diff-cleanup (context)
  ;;   "Cleanup function for Delta diff."
  ;;   (when context
  ;;     (let ((diff-buffer (plist-get context :diff-buffer)))
  ;;       (when (and diff-buffer (buffer-live-p diff-buffer))
  ;;         (kill-buffer diff-buffer)))))

  ;;
  ;; ;; Set Delta as the diff tool with proper cleanup
  ;; (setq monet-diff-tool #'mr-x/monet-delta-diff)
  ;; (setq monet-diff-cleanup-tool #'mr-x/monet-delta-diff-cleanup)
  )

;;; Agent Shell - Native Emacs buffer for Claude Code
;; ACP (Agent Client Protocol) implementation for Emacs
(use-package acp
  :ensure (:host github :repo "xenodium/acp.el"))

;; Shell infrastructure for agent-shell
(use-package shell-maker
  :ensure t)

;; Agent Shell itself
(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :after (acp shell-maker)
  :hook ((agent-shell-mode . orgtbl-mode)  ;; Auto-align org tables
         (agent-shell-mode . (lambda () (corfu-mode -1))))  ;; Disable corfu (no backend)
  :config
  ;; Enable syntax highlighting in code blocks
  (setq agent-shell-highlight-blocks t)
  ;; Custom icons
  (setq agent-shell-permission-icon "\uf259")
  (setq agent-shell-thought-process-icon "\uf29d")

  ;; Fix keybindings in agent-shell diff view (evil-mode conflicts)
  ;; The diff view uses buttons - we need to make sure evil doesn't intercept keys
  (defun mr-x/agent-shell-diff-mode-setup ()
    "Setup keybindings for agent-shell diff buffers."
    (when (string-match-p "\\*agent-shell.*diff\\*" (buffer-name))
      ;; Use emacs state so all the button keys (y/n/a/p) work properly
      (evil-emacs-state)
      ;; Also set up keys in normal mode as backup
      (evil-local-set-key 'normal (kbd "C-n") #'diff-hunk-next)
      (evil-local-set-key 'normal (kbd "C-p") #'diff-hunk-prev)
      (evil-local-set-key 'normal (kbd "j") #'diff-hunk-next)
      (evil-local-set-key 'normal (kbd "k") #'diff-hunk-prev)
      (evil-local-set-key 'normal (kbd "q") #'kill-current-buffer)
      (evil-local-set-key 'normal (kbd "RET") #'push-button)
      (evil-local-set-key 'normal (kbd "TAB") #'forward-button)
      (evil-local-set-key 'normal (kbd "<backtab>") #'backward-button)))
  (add-hook 'diff-mode-hook #'mr-x/agent-shell-diff-mode-setup)

  (defun mr-x/focus-ai-window ()
    "Focus the agent-shell window if visible, otherwise show a message."
    (interactive)
    (let ((ai-window nil))
      (walk-windows
       (lambda (w)
         (when (and (not ai-window)
                    (eq (buffer-local-value 'major-mode (window-buffer w))
                        'agent-shell-mode))
           (setq ai-window w))))
      (if ai-window
          (select-window ai-window)
        (mr-x/agent-shell-toggle))))


  ;; Auto-close diff buffer when permission is accepted/rejected
  (advice-add 'agent-shell--send-permission-response :after
              (lambda (&rest _)
                (dolist (buf (buffer-list))
                  (when (string-match-p "\\*agent-shell.*diff\\*" (buffer-name buf))
                    (kill-buffer buf)))))
  ;; Use existing Claude CLI login
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  ;; Inherit environment (gets PATH, ANTHROPIC_API_KEY, etc.)
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))

  ;; Custom prompt config
  (setq agent-shell-preferred-agent-config
        (agent-shell-make-agent-config
         :mode-line-name "Claude"
         :buffer-name "Claude"
         :shell-prompt "λ "
         :shell-prompt-regexp "λ "
         :icon-name "anthropic.png"
         :welcome-function #'agent-shell-anthropic--claude-code-welcome-message
         :client-maker (lambda (buffer)
                         (agent-shell-anthropic-make-claude-client :buffer buffer))
         :default-model-id (lambda () agent-shell-anthropic-default-model-id))))


;; Agent Shell Sidebar - Project-isolated sidebar for agent-shell
(use-package agent-shell-sidebar
  :ensure (:host github :repo "cmacrae/agent-shell-sidebar")
  :after agent-shell
  :config
  ;; Use the same config as agent-shell-preferred-agent-config
  (setq agent-shell-sidebar-default-config agent-shell-preferred-agent-config)
  ;; Position sidebar on the left (options: left, right)
  (setq agent-shell-sidebar-side 'left)
  (setq agent-shell-sidebar-locked nil)
  ;; Width as percentage of frame
  (setq agent-shell-sidebar-width "5%"))


;; Agent Shell Attention - Mode-line indicator for pending agent buffers
(use-package agent-shell-attention
  :ensure (:host github :repo "ultronozm/agent-shell-attention.el")
  :after agent-shell
  :config
  ;; Define a custom face for the indicator
  (defface agent-shell-attention-face
    '((t :foreground "#fb4934" :weight bold))  ;; gruvbox red
    "Face for agent-shell-attention mode-line indicator.")

  ;; Override the render function to use our face
  (defun mr-x/agent-shell-attention-render (pending _active)
    "Render with custom face. PENDING is count, _ACTIVE ignored."
    (when (or agent-shell-attention-show-zeros (not (zerop pending)))
      (propertize (format agent-shell-attention-lighter pending)
                  'face 'agent-shell-attention-face
                  'mouse-face 'mode-line-highlight
                  'local-map agent-shell-attention--mode-line-map)))

  (setq agent-shell-attention-render-function #'mr-x/agent-shell-attention-render)

  ;; macOS notifications via AppleScript
  (defun mr-x/agent-shell-notify (_buffer title body)
    "Send macOS notification via AppleScript."
    (start-process "notify" nil "osascript"
                   "-e" (format "display notification %S with title %S"
                                body title)))

  (setq agent-shell-attention-notify-function #'mr-x/agent-shell-notify)
  (agent-shell-attention-mode 1))


;; Agent Shell Manager - Dashboard for managing multiple agent sessions
(use-package agent-shell-manager
  :ensure (:host github :repo "jethrokuan/agent-shell-manager")
  :after agent-shell
  :config
  ;; Position manager window (options: left, right, top, bottom, nil for no auto-display)
  (setq agent-shell-manager-window-side 'bottom))



;; (use-package claude-code
;;   :ensure (:host github :repo "stevemolitor/claude-code.el")
;;   :after general
;;   :config
;;   ;; Use vterm as the terminal backend (since you already have it)
;;   (setq claude-code-terminal-backend 'vterm)
  
;;   ;; Enable claude-code-mode
;;   (claude-code-mode 1)
  
;;   ;; Key binding for the command map - using a different prefix since you use C-c c for org-capture
;;   :bind-keymap
;;   ("C-c C-l" . claude-code-command-map)  ; or choose your preferred prefix
  
;;   ;; Optional: Set up repeat map for mode cycling
;;   :bind
;;   (:repeat-map my-claude-code-repeat-map 
;;                ("M" . claude-code-cycle-mode)))

;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)





;; Adjust frame transparency for focused reading/viewing
(defun mr-x/adjust-frame-alpha-for-focus ()
  "Make frame opaque when viewing focused content (Claude, PDF, markdown preview), transparent otherwise."
  (let ((should-be-opaque nil))
    ;; Check all windows in the current frame
    (walk-windows
     (lambda (win)
       (let ((buf (window-buffer win)))
         (when (or (string-match-p "\\*claude" (buffer-name buf))
                   (with-current-buffer buf (eq major-mode 'pdf-view-mode))
                   (with-current-buffer buf (eq major-mode 'xwidget-webkit-mode)))
           (setq should-be-opaque t))))
     nil 'visible)
    ;; Set alpha based on whether Claude or book is showing
    (if should-be-opaque
        (set-frame-parameter nil 'alpha '(100 100))   ;; opaque when reading
      (set-frame-parameter nil 'alpha '(80 50)))))    ;; transparent otherwise

(add-hook 'window-configuration-change-hook #'mr-x/adjust-frame-alpha-for-focus)

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

(use-package page-break-lines
  :ensure t)
