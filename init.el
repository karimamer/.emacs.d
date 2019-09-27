;;; config  --- my config
;;; Commentary
;; set shell

(setenv "SHELL" "/usr/local/bin/fish")

;; prevent opening screen

(setq inhibit-startup-message t) ;; hide the startup message


;; UTF-8

(prefer-coding-system 'utf-8)




;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'cherry-blossom t)

;; Set up package archives

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
 (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
 (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
 (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;;; init-use-package.el ends here

;; Pin a couple troublesome packages
(add-to-list 'package-pinned-packages '(python-mode . "melpa") t)

;; refresh packages
(when (not package-archive-contents)
  (package-refresh-contents))

;; line number
(global-linum-mode t)

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
    (fish-mode haskell-emacs intero magit projectile restart-emacs auto-package-update neotree json-mode js3-mode web-mode ## pager dashboard flycheck-mypy elpy ein better-defaults markdown-mode+ markdown-mode use-package))))
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
(setq dashboard-banner-logo-title "work setup")


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



;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)



;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)


;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))


;; reid is the best
(use-package auto-package-update
	:ensure t
	:init
	(setq auto-package-update-interval 1))

(use-package restart-emacs
	:ensure t)

;; Give me a warning
(add-hook 'auto-package-update-before-hook
          (lambda ()
            (message "Updating packages and restarting!")))

;; Kick emacs after packages are updated
(add-hook 'auto-package-update-after-hook
          (lambda ()
            (restart-emacs)))

;; Do some crazy shit to automatically rebuild the Irony server

(add-hook 'auto-package-update-after-hook
          (lambda ()
            ;; FIXME: This cmd format string is lifted directly from irony-mode.el It would be nice
            ;; if it was something irony exported I could call into.
            (let ((cmd (format
                        (concat "%s %s %s && %s --build . "
                                "--use-stderr --config Release --target install")
                        (shell-quote-argument irony-cmake-executable)
                        (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
                                                      (expand-file-name
                                                       irony-server-install-prefix)))
                        (shell-quote-argument irony-server-source-dir)
                        (shell-quote-argument irony-cmake-executable))))
              (funcall-interactively 'irony-install-server cmd))))



;; add neotree
  (add-to-list 'load-path "~/.emacs.d/neotree")
  (require 'neotree)
    (global-set-key [f8] 'neotree-toggle)

;; install to git emacs
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; Every time when the neotree window is opened, let it find current file and jump to node.

(setq neo-smart-open t)

;;; flycheck-mypy.el --- Support mypy in flycheckß
;;; Code:
(require 'flycheck)

(flycheck-def-args-var flycheck-python-mypy-args python-mypy)

(flycheck-define-checker python-mypy
  "Mypy syntax checker. Requires mypy>=0.3.1.
Customize `flycheck-python-mypy-args` to add specific args to default
executable.
E.g. when processing Python2 files, add \"--py2\".
See URL `http://mypy-lang.org/'."

  :command ("mypy"
            (eval flycheck-python-mypy-args)
            source-original)
  :error-patterns
  ((error line-start (file-name) ":" line ": error:" (message) line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)

(provide 'flycheck-mypy)
;;; flycheck-mypy.el ends here


;; LSP
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)
(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
 )


;;; init ends here
