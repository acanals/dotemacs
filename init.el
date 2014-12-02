(load "~/.emacs.d/misc/global-key-bindings")

(add-to-list 'load-path "~/.emacs.d/misc/")
(require 'yasnippet-bundle)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(require 'auto-complete)
(global-auto-complete-mode t)

;; ls dired issue on emacs 24.4
;; http://stackoverflow.com/a/4083987
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(eval-after-load 'rspec-mode
 '(rspec-install-snippets))

;; emacs configuration
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

(global-auto-revert-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(setq js-indent-level 2)

(setq org-support-shift-select 1)

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)

(setq-default ispell-program-name "aspell")
(setq-default ispell-extra-args '("--reverse"))

(push "/usr/local/bin" exec-path)

;; don't let next-line add new lines at end of file
(setq next-line-add-newlines nil)

;; make edited files end with a carriage return
(setq require-final-newline nil)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq path-to-ctags "/usr/local/bin/ctags") ;; <- your ctags path here
  (defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name (directory-file-name dir-name)))
  )

;;(find-library (file-name-sans-extension (symbol-file major-mode)))


(delete-selection-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
;; (set-fringe-style -1)
;; (tooltip-mode -1)

(set-frame-font "Menlo-18")
;; (load-theme 'tango)

(require 'ido)
(ido-mode t)

(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation)
                               (define-key ruby-mode-map (kbd "M-r") 'run-rails-test-or-ruby-buffer))))
(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode '(lambda ()
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(defun css-mode-hook ()
  (autoload 'css-mode 'load-path "~/.emacs.d/css-mode.el" nil t)
  (add-to-list 'auto-mode-alist '("\\.css$" . yaml-mode))
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-offset 4)
                              (setq css-indent-offset 4))))
(defun is-rails-project ()
  (when (textmate-project-root)
    (file-exists-p (expand-file-name "config/environment.rb" (textmate-project-root)))))

(defun run-rails-test-or-ruby-buffer ()
  (interactive)
  (if (is-rails-project)
      (let* ((path (buffer-file-name))
             (filename (file-name-nondirectory path))
             (test-path (expand-file-name "test" (textmate-project-root)))
             (command (list ruby-compilation-executable "-I" test-path path)))
        (pop-to-buffer (ruby-compilation-do filename command)))
    (ruby-compilation-this-buffer)))

(eval-after-load 'tramp
  '(vagrant-tramp-enable))

;; (global-set-key "\C-x\C-m" 'execute-extended-command)

;; (add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
;; (color-theme-calm-forest)

(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))

;; Resize Fonts
(defun sacha/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun sacha/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))
(global-set-key (kbd "C-+") 'sacha/increase-font-size)
(global-set-key (kbd "C--") 'sacha/decrease-font-size)

(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jst$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; SMEX Mode

;; (require 'smex) 
;; (smex-initialize)

;; (global-set-key (kbd "\C-x\C-m") 'smex) (global-set-key (kbd "M-X") 'smex-major-mode-commands) 
;; ;; This is your old M-x. (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; (global-set-key [(meta x)] (lambda ()
;;                              (interactive)
;;                              (or (boundp 'smex-cache)
;;                                  (smex-initialize))
;;                              (global-set-key [(meta x)] 'smex)
;;                              (smex)))

;; (global-set-key [(shift meta x)] (lambda ()
;;                                    (interactive)
;;                                    (or (boundp 'smex-cache)
;;                                        (smex-initialize))
;;                                    (global-set-key [(shift meta x)] 'smex-major-mode-commands)
;;                                    (smex-major-mode-commands)))

;;(load "/Users/acanals/.emacs.d/nxhtml/autostart.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes (quote ("3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" default)))
 '(fci-rule-color "#383838")
 '(js2-auto-indent-p t)
 '(js2-cleanup-whitespace t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; Wild Stuff

(defun detect-major-mode ()
  (interactive)
  (find-library (file-name-sans-extension (symbol-file major-mode)))
  (global-set-key (kbd "C-c m") 'detect-major-mode))

(global-set-key (kbd "C-c m") (lambda ()
                                (interactive)
                                (find-library (file-name-sans-extension (symbol-file major-mode)))
                                ))

(put 'narrow-to-region 'disabled nil)
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(require 'mmm-erb)
(require 'mmm-auto)
(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)
(put 'autopair-newline 'disabled nil)

;; hl-line mode

(global-hl-line-mode 1)

 (defun hl-line-toggle-when-idle (&optional arg)
    "Turn on or off using `global-hl-line-mode' when Emacs is idle.
    When on, use `global-hl-line-mode' whenever Emacs is idle.
    With prefix argument, turn on if ARG > 0; else turn off."
      (interactive "P")
      (setq hl-line-when-idle-p
            (if arg (> (prefix-numeric-value arg) 0) (not hl-line-when-idle-p)))
      (cond (hl-line-when-idle-p
             (timer-activate-when-idle hl-line-idle-timer)
             (message "Turned ON using `global-hl-line-mode' when Emacs is idle."))
            (t
             (cancel-timer hl-line-idle-timer)
             (message "Turned OFF using `global-hl-line-mode' when Emacs is idle."))))
    
    (defun hl-line-highlight-now ()
      "Turn on `global-hl-line-mode' and highlight current line now."
      (unless global-hl-line-mode
        (global-hl-line-mode 1)
        (global-hl-line-highlight)
        (add-hook 'pre-command-hook 'hl-line-unhighlight-now)
        ))
    
    (defun hl-line-unhighlight-now ()
      "Turn off `global-hl-line-mode' and unhighlight current line now."
      (global-hl-line-mode -1)
      (global-hl-line-unhighlight)
      (remove-hook 'pre-command-hook 'hl-line-unhighlight-now))

(add-hook 'term-mode-hook (lambda()
        (setq yas-dont-activate t)))


(setq css-indent-offset 2)
(setq ruby-indent-level 2)
