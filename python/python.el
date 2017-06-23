;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(python-mode . "melpa") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)



;; yasnippet, a tool for writing text macros

(use-package yasnippet
	:ensure t
	:init
	(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

	:config
	(yas-reload-all)
	(add-hook 'prog-mode-hook 'yas-minor-mode))

(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
         (nr 0)
         (formatted-args
          (mapconcat
           (lambda (x)
             (concat "   " (nth 0 x)
                     (if make-fields (format " ${%d:arg%d}" (incf nr) nr))
                     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
           args
           indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat #'identity
                  (list "" "Args:" formatted-args)
                  indent)
       "\n"))))

(defun python-args-to-sphinx-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (make-string (current-column) 32))
         (args (python-split-args text))
         (nr 0))
    (mapconcat
     (lambda (x)
       (concat ""
               (if make-fields (format ":param ${%d:%s}: $%d" (incf nr) (nth 0 x) (incf nr)) "")
               (if (nth 1 x) (concat " \(default " (nth 1 x) "\)\n") "\n")
               (if make-fields (format "%s:type ${%d:%s}: $%d\n" indent (incf nr) (nth 0 x) (incf nr)) "")))
     args
     indent)))
