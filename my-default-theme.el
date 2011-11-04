(deftheme my-default
  "Created 2010-02-16.")

(custom-theme-set-variables
 'my-default
'(iswitchb-mode t)
 '(blink-cursor-interval 0.3)
 '(column-number-mode t)
 '(display-battery-mode nil)
 '(fringe-mode nil)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode nil)
 '(tab-width 4)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward)))

(custom-theme-set-faces
 'my-default
 '(default ((t (:family "Liberation Mono" :foundry "outline" :width normal :height 113 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "LightSalmon" :background "grey15" :stipple nil))))
 '(completions-first-difference ((t (:foreground "yellow" :inherit (bold)))))
 '(cursor ((t (:inverse-video t :background "red"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "grey60"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:foreground "grey80")))))

(provide-theme 'my-default)
