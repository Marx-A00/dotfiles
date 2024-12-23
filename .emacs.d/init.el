;;; -*- lexical-binding: t; -*-

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

(setq confirm-kill-emacs #'yes-or-no-p)

    ;; no large file warning
  (setq large-file-warning-threshold nil)

  (setq warning-minimum-level :emergency)

    (use-package exec-path-from-shell
      :ensure t
      :init
      (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

    (require 'transient)

    (setq auth-sources '("~/.authinfo.gpg"))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun mr-x/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		     (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'mr-x/display-startup-time)

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
 ;; Makes sure to download new packages if they aren't already downloaded
 use-package-always-ensure t
 use-package-verbose t)
;; Package install logging. Packages break, it's nice to know why.

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
    :ensure t
    :config
    (defconst dracula-background "#282a36")
    (defconst dracula-current-line "#44475a")
    (defconst dracula-selection "#44475a")
    (defconst dracula-foreground "#f8f8f2")
    (defconst dracula-comment "#6272a4")
    (defconst dracula-cyan "#8be9fd")
    (defconst dracula-green "#50fa7b")
    (defconst dracula-orange "#ffb86c")
    (defconst dracula-pink "#ff79c6")
    (defconst dracula-purple "#bd93f9")
    (defconst dracula-red "#ff5555")
    (defconst dracula-yellow "#f1fa8c")

    (setq vterm-color-black   dracula-background)
    (setq vterm-color-red     dracula-red)
    (setq vterm-color-green   dracula-green)
    (setq vterm-color-yellow  dracula-yellow)
    (setq vterm-color-blue    dracula-purple)
    (setq vterm-color-magenta dracula-pink)
    (setq vterm-color-cyan    dracula-cyan)
    (setq vterm-color-white   dracula-foreground))

    ;; Ensure TERM is set correctly
    (setq vterm-term-environment-variable "xterm-256color")


    ;; Optional: set the shell explicitly if needed
    ;; (setq vterm-shell "/bin/zsh")

 (custom-set-faces
;; Standard colors
'(vterm-color-black ((t (:foreground "#282a36" :background "#282a36"))))
'(vterm-color-red ((t (:foreground "#ff5555" :background "#ff5555"))))
'(vterm-color-green ((t (:foreground "#50fa7b" :background "#50fa7b"))))
'(vterm-color-yellow ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
'(vterm-color-blue ((t (:foreground "#bd93f9" :background "#bd93f9"))))
'(vterm-color-magenta ((t (:foreground "#ff79c6" :background "#ff79c6"))))
'(vterm-color-cyan ((t (:foreground "#8be9fd" :background "#8be9fd"))))
'(vterm-color-white ((t (:foreground "#f8f8f2" :background "#f8f8f2"))))

;; Bright colors
'(vterm-color-bright-black ((t (:foreground "#6272a4" :background "#6272a4"))))
'(vterm-color-bright-red ((t (:foreground "#ff6e6e" :background "#ff6e6e"))))
'(vterm-color-bright-green ((t (:foreground "#69ff94" :background "#69ff94"))))
'(vterm-color-bright-yellow ((t (:foreground "#ffffa5" :background "#ffffa5"))))
'(vterm-color-bright-blue ((t (:foreground "#d6acff" :background "#d6acff"))))
'(vterm-color-bright-magenta ((t (:foreground "#ff92df" :background "#ff92df"))))
'(vterm-color-bright-cyan ((t (:foreground "#a4ffff" :background "#a4ffff"))))
'(vterm-color-bright-white ((t (:foreground "#ffffff" :background "#ffffff")))))




  (use-package multi-vterm
	:config
	(add-hook 'vterm-mode-hook
			(lambda ()
			(setq-local evil-insert-state-cursor 'box)
			(evil-insert-state)))

	(define-key vterm-mode-map [return]                      #'vterm-send-return)

	(setq vterm-keymap-exceptions nil))





  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  ;; 	;(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  ;; 	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  ;; 	(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  ;; 	(evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  ;; 	(evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  ;; 	(evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  ;; 	(evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  ;; 	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  ;; 	(evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)



  ;; ;; (set-face-attribute 'vterm-color-black nil 
  ;; 		     :background dracula-background :foreground dracula-background)
  ;; (set-face-attribute 'vterm-color-red nil 
  ;; 		     :background dracula-red :foreground dracula-red)
  ;; (set-face-attribute 'vterm-color-green nil 
  ;; 		     :background dracula-green :foreground dracula-green)
  ;; (set-face-attribute 'vterm-color-yellow nil 
  ;; 		     :background dracula-yellow :foreground dracula-yellow)
  ;; (set-face-attribute 'vterm-color-blue nil 
  ;; 		     :background dracula-purple :foreground dracula-purple)
  ;; (set-face-attribute 'vterm-color-magenta nil 
  ;; 		     :background dracula-pink :foreground dracula-pink)
  ;; (set-face-attribute 'vterm-color-cyan nil 
  ;; 		     :background dracula-cyan :foreground dracula-cyan)
  ;; (set-face-attribute 'vterm-color-white nil 
  ;; 		     :background dracula-foreground :foreground dracula-foreground)

(setq visible-bell t)

    (set-face-attribute 'default nil :font "Iosevka" :height 280)

    (use-package all-the-icons
      :if (display-graphic-p))


    (use-package rainbow-delimiters
      :hook (prog-mode . rainbow-delimiters-mode))

    (use-package doom-themes)
    (load-theme 'doom-gruvbox)

  (use-package doom-modeline
    :init (doom-modeline-mode 1))


;;(defun task-tracker-for-modeline()
;;  "Return a string"
;; (format  "Tasks: %d/%d" tasks-completed-for-day tasks-goal-for-day))


(setq doom-modeline-modal-modern-icon nil)
(setq doom-modeline-persp-name t)
(setq org-clock-string-limit 20)

    (use-package which-key
      :defer 0
      :config
      (which-key-mode)
      (setq which-key-idle-delay 1))




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
    :ensure t 
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

;; (use-package evil-goggles
;;   :ensure t
;;   :config
;;   (evil-goggles-mode))

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
    :hook (ledger-mode . company-mode)
    :bind (:map company-active-map
		("<tab>" . company-complete-selection))
    (:map lsp-mode-map
	  ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)    ;; Minimum prefix length for completion
    (company-idle-delay 0.0)           ;; Delay before completion starts
    (global-company-mode))

(global-set-key (kbd "s-<return>") #'lsp-ui-peek-find-definitions)




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
  :init (pdf-loader-install))

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

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode))

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
  :commands magit-status
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
  "h" 'winner-undo
  "l" 'winner-redo
  "s" 'mr-x/toggle-shortcuts
  "S" 'mr-x/scratch
  "v" 'multi-vterm
  "b" 'persp-counsel-switch-buffer
  "e" '(lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/.emacs.d/emacs.org")))
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

(use-package evil-snipe
      :ensure t
      :after evil
      :config
      (evil-snipe-mode +1)
      (evil-snipe-override-mode +1))

(defun my-center-cursor-line ()
    (recenter))

(define-key evil-normal-state-map (kbd "C-d") (lambda ()
						(interactive)
						(evil-scroll-down nil)
						(my-center-cursor-line)))

(define-key evil-normal-state-map (kbd "C-u") (lambda ()
						(interactive)
						(evil-scroll-up nil)
						(my-center-cursor-line)))

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
  :after ivy
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
      (setq org-fold-core-style 'overlays)
      (setq org-agenda-span 'day)
      (setq evil-auto-indent nil))

  (setq org-agenda-files
	'("~/roaming/agenda.org"
	  "~/roaming/habits.org"
	  "~/jira"))

  ; Animation support

  (add-hook 'org-mode-hook #'org-inline-anim-mode)

;; Org store link at point from chat gpt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun org-store-link-at-point ()					        ;;
;;   "Store a link at the current buffer position, including line and column."  ;;
;;   (let ((filename (buffer-file-name))				        ;;
;;         (line-number (line-number-at-pos))				        ;;
;;         (col-number (current-column)))				        ;;
;;     (org-store-link-props						        ;;
;;      :type "file+line+column"					        ;;
;;      :link (format "file:%s::%d:%d" filename line-number col-number)	        ;;
;;      :description (format "Link to %s (line %d, col %d)"		        ;;
;;                           (file-name-nondirectory filename)		        ;;
;;                           line-number col-number))))			        ;;
;; 									        ;;
;; ;; Set this function to `org-store-link' hook for customized link storing.   ;;
;; (org-link-set-parameters "file+line+column" :store 'org-store-link-at-point) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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
