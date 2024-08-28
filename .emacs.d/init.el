;;; -*- lexical-binding: t; -*-

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

(setq confirm-kill-emacs #'yes-or-no-p)

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(require 'transient)

(setq auth-sources '("~/.authinfo.gpg"))

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

(use-package vterm
  :ensure t)

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


;;(defun task-tracker-for-modeline()
;;  "Return a string"
;; (format  "Tasks: %d/%d" tasks-completed-for-day tasks-goal-for-day))


(setq doom-modeline-modal-modern-icon nil)
(setq doom-modeline-persp-name t)

(setq mode-line-misc-info
      '((which-function-mode (which-func-mode ("" which-func-format " ")))
       ;; (:eval (task-tracker-for-modeline))
	(:eval (propertize " " 'display '(space :width 1)))
	(:eval (persp-mode-line))))

    (use-package which-key ; should be moved
      :init (which-key-mode)
      :config
      (setq which-key-idle-delay 0.3))

(use-package multi-vterm
      :config
      (add-hook 'vterm-mode-hook
		      (lambda ()
		      (setq-local evil-insert-state-cursor 'box)
		      (evil-insert-state)))
      (define-key vterm-mode-map [return]                      #'vterm-send-return)

      (setq vterm-keymap-exceptions nil)
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





    (defun mr-x/general-setup ()
      (display-line-numbers-mode 1)
      (set-frame-parameter (selected-frame) 'alpha '(80 50)))

    (add-hook 'text-mode-hook #'mr-x/general-setup)
    (add-hook 'prog-mode-hook #'mr-x/general-setup)

					    ; opacity
    (set-frame-parameter (selected-frame) 'alpha '(80 50))
    (add-to-list 'default-frame-alist '(alpha-background . 20))
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

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  ;; (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  ;; (ispell-hunspell-add-multi-dic "de_DE,de_CH,en_GB,en_US")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
;;(unless (file-exists-p ispell-personal-dictionary)
 ;; (write-region "" nil ispell-personal-dictionary nil 0))

(use-package popper
    :ensure t ; or :straight t
    :bind (("C-`"   . popper-toggle)
	   ("M-`"   . popper-cycle)
	   ("C-M-`" . popper-toggle-type)
	   ("C-~" . popper-kill-latest-popup))
    :init
    (setq popper-reference-buffers
	  '("\\*Messages\\*"
	    "\\*Output\\*$"
	    "^keybindings-shortcuts-and-descriptions\.org$"
	    help-mode
	    compilation-mode
	    "main-diary\\.org$" 
	    "\\*Backtrace\\*"
	    "\\*devdocs\\*"
	    "\\*Warnings\\*"
	    "\\*Help\\*"
	    "\\*vterm.*\\*"
	    "\\*vterminal.*\\*"
	    "\\*Ibuffer*\\*"
	    "\\*Helpful Function:.*\\*" ; Helpful buffers
	    "\\*Helpful Variable:.*\\*"
	    "\\*Helpful Command:.*\\*"
	    "\\*Helpful Key:.*\\*"))

    (popper-mode +1)
    (popper-echo-mode +1))                ; For echo area hints

(setq popper-group-function #'popper-group-by-perspective)

  ;; Custom function to toggle vterm with popper
  (defun mr-x/toggle-shortcuts ()
    "Toggle a buffer in a popper window that quickly displays shortcuts."
    (interactive)
    (let (shortcuts-buffer (get-buffer "keybindings-shortcuts-and-descriptions.org"))
    (if shortcuts-buffer
	(popper-toggle)
	(find-file "~/roaming/notes/applications/emacs/keybindings-shortcuts-and-descriptions.org"))))

  ;; Custom function to toggle vterm with popper
  (defun mr-x/toggle-vterm ()
    "Toggle a vterm buffer in a popper window."
    (interactive)
    (let ((vterm-buffer (get-buffer "*vterm*")))
      (if vterm-buffer
	  (popper-toggle-latest)
	(vterm))))


  ;; Bind the custom function to a key
  (global-set-key (kbd "C-c s") 'toggle-shortcuts)

(use-package beacon
  :init
  (beacon-mode)
  :config
  (setq beacon-blink-when-window-scrolls nil
	beacon-blink-when-window-changes t))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 3))

(use-package link-hint
  :ensure t
  :defer t)

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

(defun mr-x/PDFviewSetup()
      "preparation function for PDFView"

    (global-display-line-numbers-mode nil)
    (display-line-numbers-mode -1) 
    (set-frame-parameter (selected-frame) 'alpha '(100 50)))




(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(setq auto-mode-alist
      (remove  '("\\.\\(?:PDF\\|EPUB\\|CBZ\\|FB2\\|O?XPS\\|DVI\\|OD[FGPST]\\|DOCX\\|XLSX?\\|PPTX?\\|pdf\\|epub\\|cbz\\|fb2\\|o?xps\\|djvu\\|dvi\\|od[fgpst]\\|docx\\|xlsx?\\|pptx?\\)\\'" . doc-view-mode-maybe) auto-mode-alist))
(add-hook 'pdf-view-mode-hook #'mr-x/PDFviewSetup)

(use-package pdf-tools
      :defer t)

    (use-package org-noter
  :config
  ;; Your org-noter config ........
  (require 'org-noter-pdftools))

  (use-package org-pdftools
    :hook (org-mode . org-pdftools-setup-link))

  (use-package org-noter-pdftools
    :after org-noter
    :config
    ;; Add a function to ensure precise note is inserted
    (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
      (interactive "P")
      (org-noter--with-valid-session
       (let ((org-noter-insert-note-no-questions (if toggle-no-questions
						     (not org-noter-insert-note-no-questions)
						   org-noter-insert-note-no-questions))
	     (org-pdftools-use-isearch-link t)
	     (org-pdftools-use-freepointer-annot t))
	 (org-noter-insert-note (org-noter--get-precise-info)))))

    ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
    (defun org-noter-set-start-location (&optional arg)
      "When opening a session with this document, go to the current location.
  With a prefix ARG, remove start location."
      (interactive "P")
      (org-noter--with-valid-session
       (let ((inhibit-read-only t)
	     (ast (org-noter--parse-root))
	     (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
	 (with-current-buffer (org-noter--session-notes-buffer session)
	   (org-with-wide-buffer
	    (goto-char (org-element-property :begin ast))
	    (if arg
		(org-entry-delete nil org-noter-property-note-location)
	      (org-entry-put nil org-noter-property-note-location
			     (org-noter--pretty-print-location location))))))))
    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(defun mr-x/scratch ()
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
	bufname)
    (while (progn
	     (setq bufname (concat "*scratch"
				   (if (= n 0) "" (int-to-string n))
				   "*"))
	     (setq n (1+ n))
	     (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (org-mode)
    (insert (format "This is scratch buffer number %d" (- n 1)))
    (if (= n 1) initial-major-mode))) ; 1, because n was incremented

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "\
# This is a scratch org buffer.")


(defun mr-x/js-scratch ()
  "Create and switch to a JavaScript scratch buffer with a basic template."
  (interactive)
  (let ((buf (generate-new-buffer "*JS Scratch*")))
    (switch-to-buffer buf)
    (org-mode)  ; Ensure you have js-mode installed or use javascript-mode as appropriate
    (insert "#+begin_src js :results output")
    (insert "\n")
    (insert "\n")
    (insert "\n")
    (insert "#+end_src")
    (goto-char 32)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

(use-package magit
  :ensure t
  :config
(setq magit-view-git-manual-method 'man))

(use-package general
:ensure t
:config
;; allow for shorter bindings -- e.g., just using things like nmap alone without general-* prefix
(general-evil-setup t)

;; To automatically prevent Key sequence starts with a non-prefix key errors without the need to
;; explicitly unbind non-prefix keys, you can add (general-auto-unbind-keys) to your configuration
;; file. This will advise define-key to unbind any bound subsequence of the KEY. Currently, this
;; will only have an effect for general.el key definers. The advice can later be removed with
;; (general-auto-unbind-keys t).
(general-auto-unbind-keys)


(general-create-definer mr-x/leader-def
  :states '(normal visual motion emacs insert)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC"))

(mr-x/leader-def
  "d" 'diary-show-all-entries
  "a" 'mr-x/org-agenda-day
  "m" 'mu4e
  "f" 'link-hint-open-link
  "p" 'projectile-command-map
  "s" 'mr-x/toggle-shortcuts
  "S" 'mr-x/scratch
  "v" 'multi-vterm
  "b" 'persp-counsel-switch-buffer
  "1" (lambda () (interactive) (persp-switch-by-number 1))
  "2" (lambda () (interactive) (persp-switch-by-number 2))
  "3" (lambda () (interactive) (persp-switch-by-number 3))
  "4" (lambda () (interactive) (persp-switch-by-number 4))
  "5" (lambda () (interactive) (persp-switch-by-number 5)))

(defun mr-x/org-agenda-day ()
  (interactive)
  (org-agenda nil "a"))

(use-package evil
	:init (setq evil-want-integration t)
	(setq evil-want-keybinding nil)
	(setq evil-want-C-u-scroll t)
	(setq evil-want-C-i-jump nil)
	(setq evil-respect-visual-line-mode t)
	:config
	(evil-mode 1))

      (defun my-evil-ex-put ()
      "Execute the ':put' Ex command without needing to manually press RET."
      (interactive)
      (evil-ex "put")
      (execute-kbd-macro (kbd "RET")))


      (evil-define-key 'normal evil-ex-shortcut-map (kbd "s-<down> RET") (kbd ":put <RET>"))
    ; give up, figure it out later


  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
  (kbd "RET") 'org-fc-review-flip
  (kbd "n") 'org-fc-review-flip
  (kbd "s") 'org-fc-review-suspend-card
  (kbd "q") 'org-fc-review-quit)

(evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
  (kbd "a") 'org-fc-review-rate-again
  (kbd "h") 'org-fc-review-rate-hard
  (kbd "g") 'org-fc-review-rate-good
  (kbd "e") 'org-fc-review-rate-easy
  (kbd "s") 'org-fc-review-suspend-card
  (kbd "q") 'org-fc-review-quit)






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

  (use-package evil-owl
    :config
    (setq evil-owl-max-string-length 500)
    (add-to-list 'display-buffer-alist
		 '("*evil-owl*"
		   (display-buffer-in-side-window)
		   (side . bottom)
		   (window-height . 0.3)))
    (evil-owl-mode))

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
(add-hook 'dired-mode-hook
	  (lambda () (dired-hide-details-mode 1)))


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
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) "))

;; Taken from emacswiki to search for symbol/word at point
;; Must be done at end of init I guess
;; (define-key swiper-map (kbd "C-.")
;; 	    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))

;; (define-key swiper-map (kbd "M-.")
;; 	    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))

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
("C-x C-i" . persp-ibuffer)
:custom
(persp-mode-prefix-key (kbd "C-x M-x"))  ; pick your own prefix key here
:init
(persp-mode))

(defun mr-x/org-mode-setup()

    (visual-line-mode 1)
    (auto-fill-mode 0)
    (setq org-agenda-include-diary t)
    (setq org-agenda-span 'day)
    (setq evil-auto-indent nil))

(setq org-agenda-files
      '("~/roaming/agenda.org"
	"~/roaming/habits.org"))

; Animation support

(add-hook 'org-mode-hook #'org-inline-anim-mode)




;;  (defvar tasks-goal-for-day 5 "goal number of tasks for a day")
;;  (defvar tasks-completed-for-day 0 "actual number of tasks completed for a day")
;;  (defvar last-check-date (calendar-current-date))



;;  (defun reset-task-variables-on-day-change()
;;    "resets"
;;    (unless (equal org-agenda-current-date last-check-date)
;;      (setq last-check-date org-agenda-current-date)
;;      (setq tasks-completed-for-day 0)
;;      (message "task tracker date has been reset")))


;;    (defun mr-x/task-counter ()
;; "Simple function to track number of tasks completed in a given day."
;; (interactive)
;; ;; Ensure reset-task-variables-on-day-change is defined
;; (reset-task-variables-on-day-change)
;; ;; Increment the counter
;; (cl-incf tasks-completed-for-day)
;; ;; Check if the task goal has been met
;; (if (>= tasks-completed-for-day tasks-goal-for-day)
;;     (message "Congrats!! You met your task completion goal for today")
;;   (progn
;;     (sit-for 2)
;;     (message "Tasks completed today: %d/%d" tasks-completed-for-day tasks-goal-for-day)
;;     (sit-for 2))))


;;  (add-to-list 'org-after-todo-state-change-hook
;; 	   (lambda ()
;; 	     (when (equal org-state "DONE")
;; 	       (mr-x/task-counter))))

 (setq org-clock-persist 'history)
 (org-clock-persistence-insinuate)

(use-package org
      :hook (org-mode . mr-x/org-mode-setup)
      :config
      (setq org-hide-emphasis-markers t)
      (setq org-agenda-start-with-log-mode t)
      (setq org-log-done 'time)
      (setq org-log-into-drawer t)

      (general-define-key
       :keymaps 'org-mode-map
       "C-c t" 'org-insert-todo-heading)

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


   (require 'org-bullets)
  (setq org-bullets-face-name (quote org-bullet-face))
  (setq org-bullets-bullet-list
	'("🃏" "⡂" "⡆" "⢴" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))

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

(org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (js . t)
       (typescript . t)
       (sqlite . t)
       (latex . t)
       (python . t)))

(setq org-babel-python-command "python3")


    ; structure templates
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
		      (expand-file-name "~/.dotfiles/.emacs.d/emacs.org"))
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

 (my/org-roam-refresh-agenda-list)

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
  "disabled for now")

(remove-hook
 'org-after-todo-state-change-hook
 'my/org-roam-copy-todo-to-today)
;;   (defun my/org-roam-copy-todo-to-today ()
;;   (interactive)
;;   (let ((org-refile-keep t) ;; Set this to nil to delete the original!
;;      (org-roam-dailies-capture-templates
;;        '(("t" "tasks" entry "%?"
;;    	  :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
;;      (org-after-refile-insert-hook #'save-buffer)
;;      today-file
;;      pos)
;;     (save-window-excursion
;;       (org-roam-dailies--capture (current-time) t)
;;       (setq today-file (buffer-file-name))
;;       (setq pos (point)))

;;     ;; Only refile if the target file is different than the current file
;;     (unless (equal (file-truename today-file)
;;    		(file-truename (buffer-file-name)))
;;       (org-refile nil nil (list "Tasks" today-file nil pos)))))

;; (add-to-list 'org-after-todo-state-change-hook
;;    	  (lambda ()
;;    	    (when (equal org-state "DONE")
;;    	      (my/org-roam-copy-todo-to-today))))

(use-package esup
:ensure t
;; To use MELPA Stable use ":pin melpa-stable",
:pin melpa)

(use-package projectile
:diminish projectile-mode
:config (projectile-mode)
:custom ((projectile-completion-system 'ivy))
:bind (:map projectile-command-map ("v" . multi-vterm-project))
:bind-keymap
("C-c p" . projectile-command-map)
:init
;; NOTE: Set this to the folder where you keep your Git repos!
(when (file-directory-p "~/code/projects")
  (setq projectile-project-search-path '("~/code/projects")))
(setq projectile-switch-project-action #'projectile-dired))

(use-package devdocs
  :ensure t
  :config
  ;; Optional: Set default settings
  (setq devdocs-browser 'eww) ;; Use eww as the default browser
  (setq devdocs-offline-data-path "~/.emacs.d/devdocs")) ;; Directory for offline data
(global-set-key (kbd "C-h D") 'devdocs-lookup)

(electric-pair-mode 1)
(global-set-key (kbd "s-b") #'treemacs)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))

(use-package typescript-mode
	:mode "\\.ts\\'"
	:hook (typescript-mode . lsp-deferred)
	:config
	(setq typescript-indent-level 2))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(setq web-mode-enable-engine-detection t)

(use-package prettier-js
  :ensure t)

(use-package add-node-modules-path
  :ensure t
  :config
  (setq add-node-modules-path-debug t)
  (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\"")))

(eval-after-load 'web-mode
  '(progn
     (add-hook 'web-mode-hook #'add-node-modules-path)
     (add-hook 'web-mode-hook #'prettier-js-mode)))

;; emmet mode
(require 'emmet-mode)
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode) 
  (add-hook 'css-mode-hook  'emmet-mode))

;; web mode
(require 'web-mode)
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

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(use-package lsp-pyright
:ensure t
:hook (python-mode . (lambda ()
		       (require 'lsp-pyright)
			(lsp)))
:init
(when (executable-find "python3")
  (setq lsp-pyright-python-executable-cmd "/Users/marcosandrade/miniconda3/envs/openpair/bin/python3")))
(setq lsp-log-io t)


(setq lsp-pyright-venv-path "/Users/marcosandrade/miniconda3/envs")

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))

(use-package pyvenv
:ensure t
:config
(pyvenv-mode t)

;; Set correct Python interpreter
(setq pyvenv-post-activate-hooks
      (list (lambda ()
	      (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
(setq pyvenv-post-deactivate-hooks
      (list (lambda ()
	      (setq python-shell-interpreter "python3")))))

(use-package rbenv
  :ensure t
  :config
  (setq rbenv-installation-dir "~/rbenv"))

(global-rbenv-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (lsp-enable-which-key-integration t))
(add-hook 'prog-mode-hook #'lsp)

(setq lsp-warn-no-matched-clients nil)

(load "~/.emacs_secrets.el")
(setq-default gptel-model "gpt-4"
	      gptel-api-key (getenv "GPT_API_KEY")
	      gptel-default-mode 'org-mode)

(gptel-make-anthropic "Claude"          ;Any name you want
:stream t                             ;Streaming responses
:key (getenv "ANTHRO_API_KEY"))

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

(use-package hydra)
(use-package org-fc
  :load-path "~/src/org-fc"
  :custom (org-fc-directories '("~/roaming/flashcards/"))
  :config
  (require 'org-fc-hydra))

(use-package mu4e
  :ensure nil
  :load-path "/opt/homebrew/Cellar/mu/1.12.3/share/emacs/site-lisp/mu/mu4e"
  :defer 20
  :config
  (setq mu4e-mu-binary "/opt/homebrew/bin/mu")
  (setq mu4e-headers-fields
  '( (:date          .  25)    ;; alternatively, use :human-date
     (:flags         .   6)
     (:from          .  22)
     (:subject       .  nil))) ;; alternatively, use :thread-subject


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

(defun mr-x/mu4e-copy-message-id-link ()
"Copy an 'mu4e' URL link to the message at point."
(interactive)
(let ((msg (mu4e-message-at-point)))
  (when msg
    (let ((msgid (plist-get msg :message-id)))
      (kill-new (format "[[mu4e:msgid:%s][Email Link]]" msgid))
      (message "Copied mu4e link to clipboard!")))))

      (define-key mu4e-headers-mode-map (kbd "C-c C-l") 'my/mu4e-copy-message-id-link)

(load "~/.emacs_secrets.el")

(require 'org-gcal)
(setq plstore-cache-passphrase-for-symmetric-encryption t)

(use-package org-gcal
  :config
  (setq org-gcal-client-id (getenv "GCAL_CLIENT_ID")
     org-gcal-client-secret (getenv "GCAL_SECRET") 
     org-gcal-fetch-file-alist '(("mnandrade1999@gmail.com" . "~/agenda.org"))))

(setq erc-server "irc.libera.chat"
      erc-nick "MrX"    ; 
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
	    erc-auto-query 'bury)

;emms for local files
(use-package emms
:ensure t
:config
(require 'emms-setup)
(emms-standard)
(emms-default-players))

(use-package ytdl
:ensure t)

(use-package spotify
:ensure t)
