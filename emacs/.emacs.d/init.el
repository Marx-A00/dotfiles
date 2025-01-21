
;; Package Configuration

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

;; keep things clean


(use-package no-littering
  :ensure t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

;; Still need to fix #file showing up maybe

;; Startup UI

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-sourcerer))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  (setq doom-modeline-modal-modern-icon nil))


(set-frame-parameter (selected-frame) 'alpha '(60 50))
(setq inhibit-startup-message t)
(display-line-numbers-mode t)
(set-face-attribute 'default nil :font "Iosevka" :height 280)

;; UX
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)


;; org (kinda not really)
(visual-line-mode 1)
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



;; Test

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
  
;; EVIL

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

;; test

(setq display-line-numbers-type 'relative)
(dolist (mode '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
