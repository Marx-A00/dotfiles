;;; early-init.el --- runs before the GUI/package system -*- lexical-binding: t; -*-

;; Defer garbage collection during startup for speed (restored in init.el).
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; We drive packages ourselves via use-package; don't auto-activate at startup.
(setq package-enable-at-startup nil)

;; Keep native-compilation quiet — it compiles in the background, no popups.
(setq native-comp-async-report-warnings-errors 'silent)

;; Strip UI chrome early so it never flashes in.
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here
