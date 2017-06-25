;;; flycheck-mypy.el --- Support mypy in flycheck

;; Copyright (C) 2015 Lorenzo Bolla <lbolla@gmail.com>
;;
;; Author: Lorenzo Bolla <lbolla@gmail.com>
;; Created: 14 Septermber 2015
;; Version: 1.0
;; Package-Version: 20160220.1432
;; Package-Requires: ((flycheck "0.18"))

;;; Commentary:

;; This package adds support for mypy to flycheck.  To use it, add
;; to your init.el:

;; (require 'flycheck-mypy)
;; (add-hook 'python-mode-hook 'flycheck-mode)

;; If you want to use mypy you probably don't want pylint or
;; flake8. To disable those checkers, add the following to your
;; init.el:

;; (add-to-list 'flycheck-disabled-checkers 'python-flake8)
;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
