;;; permission-separator-test.el --- Test different separator colors -*- lexical-binding: t -*-

;;; Commentary:
;; Visual test for permission UI separator colors.
;; Run M-x mr-x/permission-separator-test to see all options.
;; Pick your favorite and tell Claude the color name.

;;; Code:

(defvar mr-x/separator-color-options
  '(;; Oranges / Warm tones
    ("gruvbox-orange"     . "#fe8019")
    ("orange-dim"         . "#d65d0e")
    ("orange-soft"        . "#af3a03")
    ("peach"              . "#e8a87c")
    ("coral"              . "#e07b53")
    ("salmon"             . "#cc6666")
    ("rust"               . "#b45309")
    ("amber"              . "#d97706")
    ("tangerine"          . "#fb923c")
    ;; Other warm
    ("gruvbox-yellow"     . "#fabd2f")
    ("gold"               . "#eab308")
    ("gruvbox-red"        . "#fb4934")
    ;; Cool tones for contrast
    ("gruvbox-aqua"       . "#8ec07c")
    ("gruvbox-blue"       . "#83a598")
    ("gruvbox-purple"     . "#d3869b")
    ;; Neutrals
    ("gruvbox-gray"       . "#665c54")
    ("gruvbox-light-gray" . "#928374"))
  "Separator color options to preview.")

(defun mr-x/permission-separator-test ()
  "Display all separator color options for visual comparison."
  (interactive)
  (let ((buf (get-buffer-create "*Permission Separator Test*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Permission Separator Color Test\n" 
                            'face '(:height 1.3 :weight bold)))
        (insert (propertize "================================\n\n"
                            'face '(:foreground "#665c54")))
        (insert "Pick your favorite and tell Claude the color name.\n\n")
        
        (dolist (option mr-x/separator-color-options)
          (let ((name (car option))
                (color (cdr option)))
            ;; Color name
            (insert (propertize (format "%-20s " name)
                                'face `(:foreground ,color :weight bold)))
            (insert (propertize color 'face '(:foreground "#928374")))
            (insert "\n")
            
            ;; Mock permission UI
            (insert (propertize "   Allow Edit file?" 'face 'bold))
            (insert "\n")
            (insert (propertize "   ─────────────────────────────\n"
                                'face `(:foreground ,color)))
            (insert "   ")
            (insert (propertize "1" 'face '(:foreground "#fabd2f" :weight bold)))
            (insert "   Allow once\n")
            (insert "   ")
            (insert (propertize "2" 'face '(:foreground "#fabd2f" :weight bold)))
            (insert "   Deny\n")
            (insert "   ")
            (insert (propertize "3" 'face '(:foreground "#fabd2f" :weight bold)))
            (insert "   Always allow\n")
            (insert "\n\n"))))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

(provide 'permission-separator-test)

;;; permission-separator-test.el ends here
