;;; config-tests.el --- ERT smoke tests for Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Minimal smoke tests that verify the config loads correctly and
;; critical packages, keybindings, and custom functions are available.
;;
;; Run with:
;;   emacs --batch -l ~/.emacs.d/init.el \
;;         -l ~/.emacs.d/tests/config-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or use the alias:  etest

;;; Code:

(require 'ert)

;; ---------------------------------------------------------------------------
;; Batch-mode Elpaca synchronization
;; ---------------------------------------------------------------------------
;; In batch mode, after-init-hook doesn't fire automatically.
;; We need to process Elpaca queues so packages actually install/load.

(when noninteractive
  (when (fboundp 'elpaca-process-queues)
    (elpaca-process-queues))
  (when (fboundp 'elpaca-wait)
    (elpaca-wait)))

;; ═══════════════════════════════════════════════════════════════════════════
;; Tier 1 — "Does it boot?"
;; Core packages that should be loaded eagerly after init.
;; ═══════════════════════════════════════════════════════════════════════════

(ert-deftest config-test-evil-loaded ()
  "Evil mode and evil-collection should be loaded."
  (should (featurep 'evil))
  (should (featurep 'evil-collection)))

(ert-deftest config-test-general-loaded ()
  "General.el leader key system should be loaded."
  (should (featurep 'general)))

(ert-deftest config-test-ivy-counsel-loaded ()
  "Ivy and Counsel completion framework should be loaded."
  (should (featurep 'ivy))
  (should (featurep 'counsel)))

(ert-deftest config-test-projectile-loaded ()
  "Projectile project management should be loaded."
  (should (featurep 'projectile)))

(ert-deftest config-test-which-key-loaded ()
  "Which-key should be loaded for keybinding discoverability."
  (should (featurep 'which-key)))

(ert-deftest config-test-doom-modeline-loaded ()
  "Doom modeline should be loaded."
  (should (featurep 'doom-modeline)))

(ert-deftest config-test-doom-themes-loaded ()
  "Doom themes should be loaded."
  (should (featurep 'doom-themes)))

(ert-deftest config-test-org-loaded ()
  "Org mode should be loaded."
  (should (featurep 'org)))

(ert-deftest config-test-flycheck-available ()
  "Flycheck should be available."
  (should (fboundp 'flycheck-mode)))

(ert-deftest config-test-corfu-loaded ()
  "Corfu completion should be loaded."
  (should (featurep 'corfu)))

(ert-deftest config-test-perspective-loaded ()
  "Perspective workspace management should be loaded."
  (should (featurep 'perspective)))

(ert-deftest config-test-evil-snipe-loaded ()
  "Evil-snipe should be loaded."
  (should (featurep 'evil-snipe)))

(ert-deftest config-test-persistent-scratch-loaded ()
  "Persistent scratch should be loaded."
  (should (featurep 'persistent-scratch)))

;; Deferred packages — test that autoloads registered the commands,
;; even though the library isn't loaded yet.

(ert-deftest config-test-magit-available ()
  "Magit commands should be autoloaded (deferred package)."
  (should (fboundp 'magit-status)))

(ert-deftest config-test-lsp-available ()
  "LSP mode commands should be autoloaded (deferred package)."
  (should (fboundp 'lsp))
  (should (fboundp 'lsp-deferred)))

(ert-deftest config-test-vterm-available ()
  "Vterm should be available."
  (should (fboundp 'vterm)))

;; ═══════════════════════════════════════════════════════════════════════════
;; Tier 2 — "Are my keybindings intact?"
;; Verify SPC leader map and key groups exist.
;; ═══════════════════════════════════════════════════════════════════════════

(defun config-test--leader-map ()
  "Return the SPC leader keymap from the general override map for normal state."
  (when (and (boundp 'general-override-mode-map)
             (fboundp 'evil-get-auxiliary-keymap))
    (let ((aux (evil-get-auxiliary-keymap general-override-mode-map 'normal)))
      (and aux (lookup-key aux (kbd "SPC"))))))

(ert-deftest config-test-leader-key-exists ()
  "SPC should be bound to a keymap in evil normal state."
  (let ((leader-map (config-test--leader-map)))
    (should leader-map)
    (should (keymapp leader-map))))

(ert-deftest config-test-leader-key-groups ()
  "Core leader key groups should be bound under SPC."
  (let ((leader-map (config-test--leader-map)))
    (should leader-map)
    ;; Each of these should resolve to something (a command or sub-keymap)
    (should (lookup-key leader-map (kbd "a")))   ; Agenda
    (should (lookup-key leader-map (kbd "b")))   ; Buffer
    (should (lookup-key leader-map (kbd "c")))   ; Agent Shell
    (should (lookup-key leader-map (kbd "d")))   ; Dired
    (should (lookup-key leader-map (kbd "e")))   ; Edit config
    (should (lookup-key leader-map (kbd "f")))   ; Find link
    (should (lookup-key leader-map (kbd "g")))   ; Git
    (should (lookup-key leader-map (kbd "p")))   ; Projectile
    (should (lookup-key leader-map (kbd "s")))   ; Surf
    (should (lookup-key leader-map (kbd "v")))   ; Vterm
    (should (lookup-key leader-map (kbd "w")))   ; Window
    (should (lookup-key leader-map (kbd "x")))   ; Perspectives
    (should (lookup-key leader-map (kbd "P"))))) ; Project Dashboard

;; ═══════════════════════════════════════════════════════════════════════════
;; Tier 3 — "Are my custom functions defined?"
;; Only the critical ones — not an exhaustive inventory.
;; ═══════════════════════════════════════════════════════════════════════════

(ert-deftest config-test-mr-x-agent-shell-functions ()
  "Critical agent-shell custom functions should be defined."
  (should (fboundp 'mr-x/agent-shell-toggle))
  (should (fboundp 'mr-x/agent-shell-new-smart))
  (should (fboundp 'mr-x/agent-shell-roaming))
  (should (fboundp 'mr-x/focus-ai-window)))

(ert-deftest config-test-mr-x-dev-functions ()
  "Development environment functions should be defined."
  (should (fboundp 'mr-x/spawn-dev-environment))
  (should (fboundp 'mr-x/spawn-project-terminal-frame))
  (should (fboundp 'mr-x/test-environment)))

(ert-deftest config-test-mr-x-ui-functions ()
  "UI and display functions should be defined."
  (should (fboundp 'mr-x/set-font-faces))
  (should (fboundp 'mr-x/org-babel-tangle-config))
  (should (fboundp 'mr-x/new-scratch)))

(ert-deftest config-test-mr-x-git-functions ()
  "Git helper functions should be defined."
  (should (fboundp 'mr-x/magit-status-side-window)))

(ert-deftest config-test-mr-x-surf-functions ()
  "Web browsing functions should be defined."
  (should (fboundp 'mr-x/surf-web))
  (should (fboundp 'mr-x/surf-link-at-point)))

(ert-deftest config-test-org-roam-functions ()
  "Org-roam custom functions should be defined."
  (should (fboundp 'my/org-roam-find-project))
  (should (fboundp 'my/org-roam-capture-inbox))
  (should (fboundp 'my/org-roam-capture-task)))

(ert-deftest config-test-leader-definer-exists ()
  "The mr-x/leader-def definer should be defined."
  (should (fboundp 'mr-x/leader-def)))

;;; config-tests.el ends here
