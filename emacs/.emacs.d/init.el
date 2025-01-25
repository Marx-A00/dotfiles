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

;; Cleaning


(use-package no-littering
  :ensure t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

;; Still need to fix #file showing up maybe

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

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
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
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :bind (:map dired-mode-map
	("." . dired-omit-mode))
  :hook (dired-mode-hook . (lambda ()
			     (dired-hide-details-mode)
			     (dired-omit-mode)))
  :custom
  (dired-omit-files (rx (seq bol ".")))
  (setq insert-directory-program "gls")
  (setq dired-listing-switches "-al --group-directories-first")
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)
  :init
  (with-eval-after-load 'dired (require 'dired-x)))

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

;; Startup UI



;; org (kinda not really)
	(defun mr-x/org-mode-setup()

	    (visual-line-mode 1)
	    (auto-fill-mode 0)
	    (setq org-agenda-include-diary t)
	    (setq org-fold-core-style 'overlays)
	    (setq org-agenda-span 'day)
	    (setq evil-auto-indent nil))

	(setq org-agenda-files
	      '("~/roaming/agenda.org"
		"~/roaming/habits.org"
		"~/jira"))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

   (use-package org
	:hook (org-mode . mr-x/org-mode-setup)
	:config
	(setq org-hide-emphasis-markers t)
	(setq org-agenda-start-with-log-mode t)
	(setq org-log-done 'time)
	(setq org-log-into-drawer t)

	;; (general-define-key
	;;  :keymaps 'org-mode-map
	;;  "C-c t" 'org-insert-todo-heading)

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
	 '(org-level-3 ((t (:foreground "#67c0de")))))

	(setq org-refile-targets
	      '(("Archive.org" :maxlevel . 1)))

	(advice-add 'org-refile :after 'org-save-all-org-buffers))

 ;; (defun org-summary-todo (n-done n-not-done)
 ;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
 ;;   (let (org-log-done org-todo-log-states)   ; turn off logging
 ;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)


(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-headline-bullets-list
 '("🃏" "⡂" "⡆" "⢴" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (setq org-ellipsis " ‧"))



   ;;  (require 'org-bullets)
   ;; (setq org-bullets-face-name (quote org-bullet-face))


(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

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
	(setq org-agenda-window-setup 'other-window)
	(set-face-attribute 'org-agenda-date nil :height 1.1)
	(set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
	(set-face-attribute 'org-agenda-date-today nil
		      :foreground "#897d6c"   
		      :background nil        
		      :weight 'bold
		      :underline nil)           ;; Make it bold
	(set-face-attribute 'org-agenda-date-weekend nil :height 1.1))

     (setq org-agenda-breadcrumbs-separator " ❱ "
	    org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
	    org-agenda-time-grid '((weekly today require-timed)
				   (800 1000 1200 1400 1600 1800 2000)
				   "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
	    org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
				       (todo . " %i %-12:c")
				       (tags . " %i %-12:c")
				       (search . " %i %-12:c")))

     (setq org-agenda-format-date (lambda (date)
				    (concat"\n"(make-string(window-width)9472)
					   "\n"(org-agenda-format-date-aligned date))))
     (setq org-cycle-separator-lines 2)

     (add-hook 'org-agenda-finalize-hook
		(lambda ()
		  (setq visual-fill-column-width 100) 
		  (setq visual-fill-column-center-text t)
		  (visual-fill-column-mode t)))

(use-package ob-typescript
  :ensure t)

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
 (defun efs/org-babel-tangle-config ()
   (when (string-equal (buffer-file-name)
			(expand-file-name "~/.dotfiles/emacs/.emacs.d/emacs.org"))
     ;; Dynamic scoping to the rescue
     (let ((org-confirm-babel-evaluate nil))
	(org-babel-tangle))))

 (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

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

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
