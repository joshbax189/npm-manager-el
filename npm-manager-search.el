;;; npm-manager-search.el --- Search npm package registry   -*- lexical-binding: t -*-

;; Copyright (C) 2024 Josh Bax

;; Author: Josh Bax

;; Keywords: languages
;; URL: https://github.com/joshbax189/npm-manager-el

;; Package-Version: 0.1.1

;; Package-Requires: ((emacs "28.1") (aio "1.0") (dash "2.19.1") (tablist "1.1") (transient "0.7.1"))

;; This file is not part of GNU Emacs.

;; NPM manager is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; NPM manager is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with NPM manager.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Search NPM registry for packages.

;;; Code:

(require 'aio)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'transient)
(require 'url)
(require 'npm-manager)

(defvar npm-manager-search-string ""
  "The currently active search.")
(make-variable-buffer-local 'npm-manager-search-string)

(defcustom npm-manager-search-result-limit 100
  "Max number of search results to return.  Max possible value is 250."
  :type 'number
  :group 'npm-manager)

(defcustom npm-manager-search-registry-host "https://registry.npmjs.org"
  "Which NPM registry server to use."
  :type 'string
  :group 'npm-manager)

(aio-defun npm-manager-search--fetch (search-string)
 "Search for SEARCH-STRING using NPM registry API.

Returns a promise that is fulfilled with the decoded
JSON search result."
 (-let* ((registry-url
          (format "%s/-/v1/search?size=%s&text=%s"
                  npm-manager-search-registry-host
                  npm-manager-search-result-limit
                  search-string))
         (((&plist :error the-error) . res-buffer)
          (aio-await (aio-url-retrieve registry-url))))
   (if the-error
       (signal the-error)
     (with-current-buffer res-buffer
       (goto-char (point-min))
       (while (looking-at "^.")
         (forward-line))
       (prog1 (json-parse-string (buffer-substring
                                  (point) (point-max))
                                 :object-type 'alist)
         (kill-buffer))))))

(defun npm-manager-search--format-score (score-num)
  "Format SCORE-NUM for display in tablist."
  (seq-take (number-to-string score-num) 4))

(defun npm-manager-search-refresh ()
  "Refresh the contents of NPM search list display."
  (interactive)
  ;; see https://www.npmjs.com/package/libnpmsearch#api
  ;; and https://github.com/npm/registry/blob/master/docs/REGISTRY-API.md
  (let* ((result (aio-wait-for (npm-manager-search--fetch npm-manager-search-string)))
         (data (map-elt result 'objects)))
    (--map (list it
                 (let ((package (map-elt it 'package))
                       (score (map-elt it 'score)))
                   (let-alist package
                     (vector .name
                             (or .description "")
                             (or (map-elt .author 'name)
                                 "")
                             (car (split-string .date "T"))
                             .version
                             (npm-manager-search--format-score (map-nested-elt score '(detail quality)))
                             (npm-manager-search--format-score (map-nested-elt score '(detail popularity)))
                             (npm-manager-search--format-score (map-nested-elt score '(detail maintenance)))))))
           data)))

(defun npm-manager-search-info ()
  "Run `npm info` on the package at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (name (seq-elt entry 0))
         (ver (seq-elt entry 4)))
    (npm-manager--display-command "info" "" (format "%s@%s" name ver))))

(transient-define-prefix npm-manager-search-install ()
  "Search npm packages inheriting previous search state."
  :incompatible '(("--save-prod" "--save-dev" "--save-optional" "--no-save")
                  ("--save-exact" "--save-bundle" "--no-save"))
  :value '("--save-prod")
  ["Save To"
   ("-P" "prod" "--save-prod")
   ("-D" "dev" "--save-dev")
   ("-O" "optional" "--save-optional")
   ("ns" "no save" "--no-save")]

  ["Save As"
   ("-E" "exact" "--save-exact")
   ("-B" "add to bundle" "--save-bundle")
   ("-g" "global" "--global")]

  ["Extra"
   ("-f" "force fetch" "--force")
   ("-d" "dry run" "--dry-run")
   ("plo" "package lock only" "--package-lock-only")
   ("lpd" "legacy peer deps" "--legacy-peer-deps")
   ]
  [("I" "install package" npm-manager-search-install-suffix)])

(aio-defun npm-manager-search-install-suffix (package-directory)
  "Install package at point into PACKAGE-DIRECTORY."
  (interactive "D")
  (let* ((entry (tabulated-list-get-entry))
         (package-name (seq-elt entry 0))
         (arg-string (string-join (transient-args transient-current-command) " ")))
    (let ((default-directory package-directory))
      (aio-await (npm-manager--display-command "i" arg-string package-name)))))

;;;###autoload
(transient-define-prefix npm-manager-search ()
  "Search npm packages."
  :incompatible '(("is:unstable" "not:unstable")
                  ("is:insecure" "not:insecure"))

  ["Filters"
   ("a" "author" "author=")
   ("m" "maintainer" "maintainer=")
   ("@" "scope" "scope=" :prompt "Package scope: ")
   ("k" "keywords" "keywords=" :prompt "Package keywords: ")]

  ["Stability"
   ("nu" "No packages whose version is < 1.0.0" "not:unstable")
   ("u" "Only packages whose version is < 1.0.0)" "is:unstable")
   ("ni" "No packages that are insecure or have vulnerable dependencies" "not:insecure")
   ("i" "Only packages that are insecure or have vulnerable dependencies" "is:insecure" )]
  [("s" "Enter search text" npm-manager-search--search-suffix)])

(defvar npm-manager-search-user-text "" "Last input.")
(make-variable-buffer-local 'npm-manager-search-user-text)

(defun npm-manager-search--search-suffix (search-input)
  "Collect user SEARCH-INPUT string and dispatch search."
  (interactive (list (completing-read "search: "
                                      (list npm-manager-search-user-text))))
  (transient-set)
  (let* ((data (transient-args transient-current-command))
         (clean-data (--map (string-replace "=" ":" it) data))
         (full-search (string-join (cons search-input clean-data) " ")))
    (npm-manager-search-text full-search (not (string-empty-p npm-manager-search-string)) search-input)))

(defun npm-manager-search-text (search-string &optional reuse-buffer original-input)
  "Search for an NPM package using SEARCH-STRING.

If REUSE-BUFFER is non-nil replace current buffer with result.
ORIGINAL-INPUT is the user input search string without modifiers."
  (interactive "M")
  (let ((search-buffer-name (format "NPM search: %s" search-string)))
    (if reuse-buffer
        (rename-buffer search-buffer-name 't)
      ;; else
      (when (buffer-live-p search-buffer-name)
        (kill-buffer search-buffer-name)) ;; TODO or just reset the default directory to the caller's
      (pop-to-buffer search-buffer-name)
      (npm-manager-search-mode)))
  (when original-input (setq npm-manager-search-user-text original-input))
  (setq npm-manager-search-string search-string)
  (tablist-revert))

;; TODO the keymap appears incorrect in the mode help.
;; e.g. < > and s
(defvar npm-manager-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "I"         #'npm-manager-search-install)
    (define-key map "S"         #'npm-manager-search)
    (define-key map (kbd "RET") #'npm-manager-search-info)
    map)
  "Keymap for `npm-manager-search-mode'.")

(define-derived-mode npm-manager-search-mode tabulated-list-mode "NPM Search"
  "NPM search result display major mode."
  (setq tabulated-list-format [("Name" 24 t)
                               ("Description" 48 t)
                               ("Author" 18 t)
                               ("Date" 12 t)
                               ("Version" 12)
                               ("Qual" 5 t)
                               ("Pop" 5 t)
                               ("Maint" 5 t)]
        tabulated-list-padding 2
        tabulated-list-entries #'npm-manager-search-refresh)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'npm-manager-search)

;;; npm-manager-search.el ends here
