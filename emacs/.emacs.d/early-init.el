;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Fix for yabai window management - removes titlebar
;; Use 'undecorated' for sharp corners or 'undecorated-round' for rounded
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Performance optimizations
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)