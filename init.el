;; set shell

(setenv "SHELL" "/bin/bash")

;; prevent opening screen

(setq inhibit-startup-message t) ;; hide the startup message


;; UTF-8

(prefer-coding-system 'utf-8)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; Set up package archives

(require 'package)
(setq package-enable-at-startup nil)
 (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
 (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
 (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
 (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Pull in use-package for everything else

(eval-when-compile
  (require 'use-package))


;; Pin a couple troublesome packages
(add-to-list 'package-pinned-packages '(python-mode . "melpa") t)

;; refresh packages
(when (not package-archive-contents)
  (package-refresh-contents))

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (json-mode js3-mode web-mode ## pager dashboard flycheck-mypy elpy ein better-defaults markdown-mode+ markdown-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load python
(load "~/.emacs.d/python/python.el")

;; load breakline
(load "~/.emacs.d/page-break-lines/page-break-lines.el")

;; use emacs-dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))


;; Set the title
(setq dashboard-banner-logo-title "Don't get mad get RAAAAD")


(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))

;; setup flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

