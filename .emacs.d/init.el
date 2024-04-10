;;(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
  ;;(setq exec-path (append exec-path '("/opt/homebrew/bin")))

  ;; Check if `exec-path-from-shell` is not already installed
;(unless (package-installed-p 'exec-path-from-shell)
  ;; Refresh package databases
 ; (package-refresh-contents)
  ;; Install `exec-path-from-shell`
  ;(package-install 'exec-path-from-shell))

;; Initialize `exec-path-from-shell` after installation
;(when (memq window-system '(mac ns x))
 ; (exec-path-from-shell-initialize))

;;; -*- lexical-binding: t -*- 


  ;; Initialize package sources
  (require 'package)

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("org" . "https://orgmode.org/elpa/")
			   ("elpa" . "https://elpa.gnu.org/packages/")))

  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))
  (unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq
 use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.

  ;; Initialize use-package on non-Linux platforms
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (setq use-package-always-ensure t)
(use-package exec-path-from-shell
:config
(exec-path-from-shell-initialize))

; sending custom files to shadow realm
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; For backup files (filename~)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; For autosave files (#filename#)
;; Might be useless now with no-littering package
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

; NO LITTERING
(use-package no-littering)

(setq visible-bell t)


					; Font
(set-face-attribute 'default nil :font "Iosevka" :height 280)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes)
(load-theme 'doom-gruvbox)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon (display-graphic-p)))

(use-package which-key ; should be moved
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package general) ; should be moved maybe

					; opacity
(set-frame-parameter (selected-frame) 'alpha '(80 50))
					;(add-to-list 'default-frame-alist '(alpha-background . 20))
					; keybindings section
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

(use-package beacon
  :init
  (beacon-mode)
  :config
  (setq beacon-blink-when-window-scrolls t
	beacon-blink-when-window-changes t))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 3))

(require 'winner)
(winner-mode 1)

(use-package company
    :ensure t
    :after lsp-mode
    :hook (lsp-mode . company-mode)
    :bind (:map company-active-map
		("<tab>" . company-complete-selection))
    (:map lsp-mode-map
	  ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)    ;; Minimum prefix length for completion
    (company-idle-delay 0.0)           ;; Delay before completion starts
    (global-company-mode))


  ;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

  (use-package company-box
    :hook (company-mode . company-box-mode))

(use-package PDF)
(defun mr-x/PDFviewSetup()
  "preparation function for PDFView"
    (visual-line-mode -1))

(add-hook 'pdf-view-mode-hook 'mr-x/PDFviewSetup)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "\
# This is a scratch org buffer.")

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

(use-package magit
  :ensure t)
(setq magit-view-git-manual-method 'man)

(use-package evil
    :init (setq evil-want-integration t)
   (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    (setq evil-respect-visual-line-mode t)
    :config
    (evil-mode 1))

  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package dired
      :ensure nil
      :commands (dired dired-jump)
      :bind (("C-x C-j" . dired-jump))
      :custom
      (setq insert-directory-program "gls" dired-use-ls-dired t)
      (setq dired-listing-switches "-al --group-directories-first")
      :config
      (evil-collection-define-key 'normal 'dired-mode-map
	"h" 'dired-up-directory
	"l" 'dired-find-file))


    (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's
     loaded."
    ;; <add other stuff here>
    (define-key dired-mode-map [remap dired-find-file]
      'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
      'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory]
      'dired-single-up-directory))

 ; dired-single-magic-buffer
  (global-set-key [(f5)] 'dired-single-magic-buffer)
(global-set-key [(control f5)] (function
	(lambda nil (interactive)
	(dired-single-magic-buffer default-directory))))
(global-set-key [(shift f5)] (function
	(lambda nil (interactive)
	(message "Current directory is: %s" default-directory))))
(global-set-key [(meta f5)] 'dired-single-toggle-buffer-name)

  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init))




    (use-package all-the-icons-dired
      :hook (dired-mode . all-the-icons-dired-mode))

    (use-package dired-hide-dotfiles
      :hook (dired-mode . dired-hide-dotfiles-mode)
      :config
      (evil-collection-define-key 'normal 'dired-mode-map
	"H" 'dired-hide-dotfiles-mode))

; Ivy & Counsel
(use-package ivy
  :diminish
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
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(use-package counsel-dash)

(use-package perspective
:bind
("C-x C-b" . persp-counsel-switch-buffer)         ; or use a nicer switcher, see below
:custom
(persp-mode-prefix-key (kbd "C-x M-x"))  ; pick your own prefix key here
:init
(persp-mode))

(defun mr-x/org-mode-setup()

  (visual-line-mode 1)
  (auto-fill-mode 0)
  (setq org-agenda-include-diary t)
  (setq evil-auto-indent nil))

(setq org-agenda-files '("~/roaming/agenda.org"))

; Animation support

(add-hook 'org-mode-hook #'org-inline-anim-mode)

(use-package org
    :hook (org-mode . mr-x/org-mode-setup)
    :config
    (setq org-hide-emphasis-markers t)
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
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
	     "WAIT(w@/!)"
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

(require 'org-bullets)
(setq org-bullets-face-name (quote org-bullet-face))
(setq org-bullets-bullet-list '("🃏" "⡂" "⡆" "⢴" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
(setq org-ellipsis " ‧")

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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

(use-package org-timeblock)

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
  (set-face-attribute 'org-agenda-date nil :height 1.1)
  (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))

(add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

(setq org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
      org-agenda-time-grid '((weekly today require-timed)
			     (800 1000 1200 1400 1600 1800 2000)
			     "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
      org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
				 (todo . " %i %-12:c")
				 (tags . " %i %-12:c")
				 (search . " %i %-12:c")))

(setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
						    "\n"
						    (org-agenda-format-date-aligned date))))
(setq org-cycle-separator-lines 2)

; Org-babel

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (js . t)
       (latex . t)
       (python . t)))

(setq org-babel-python-command "python3")


    ; structure templates
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("C" . "comment"))
(add-to-list 'org-structure-template-alist '("js" . "src javascript"))
(add-to-list 'org-structure-template-alist '("l" . "export latex"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/emacs.org"))
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
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n+date: %U\n")
      :unnarrowed t)
     ("w" "workout" plain
      "%?"
      :if-new (file+head "workouts/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "code-notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "~/roaming/Templates/BookNoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:project")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
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

(use-package devdocs)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package lsp-pyright
:ensure t
:hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp))))  ; or lsp-deferred

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (lsp-enable-which-key-integration t))

(load "~/.emacs_secrets.el")
(setq-default gptel-model "gpt-4"
	      gptel-api-key (getenv "GPT_API_KEY")
	      gptel-default-mode 'org-mode)

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(recentf-mode 1) ; Recent files
(setq recentf-max-menu-items 25)
(global-set-key (kbd "s-r") #'recentf-open)
(ido-mode 1) ; IDO mode for navigating files and buffers

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)
(dolist (mode '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq diary-file "~/life/diary-entries/main-diary.org")

(load "~/.emacs_secrets.el")
(use-package wakatime-mode)
(setq wakatime-api-key (getenv "WAKATIME_API_KEY"))
(setq wakatime-cli-path "/opt/homebrew/bin/wakatime-cli")
(global-wakatime-mode)

(use-package mu4e
  :ensure nil
  :defer 20
  :config
  (add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.12.1/share/emacs/site-lisp/mu/mu4e")
  (setq mu4e-mu-binary "/opt/homebrew/bin/mu")


  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Maildir")

  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder "/[Gmail].Sent Mail")
  (setq mu4e-refile-folder "/[Gmail].All Mail")
  (setq mu4e-trash-folder "/[Gmail].Trash")

  (setq mu4e-maildir-shortcuts
	'(("/Inbox"             . ?i)
	  ("/[Gmail].Sent Mail" . ?s)
	  ("/[Gmail].Trash"     . ?t)
	  ("/[Gmail].Drafts"    . ?d)
	  ("/[Gmail].All Mail"  . ?a))))

(add-hook 'after-init-hook (lambda ()
			     (message "Emacs is ready! This is your test after-init hook!")
			     ))

(setq erc-server "irc.libera.chat"
      erc-nick "MrX"    ; 
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
	    erc-auto-query 'bury)
