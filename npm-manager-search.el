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
(require 'files)
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

(defun npm-manager-search--make-search-url (search-string)
  "Make full search URL from SEARCH-STRING."
  (format "%s/-/v1/search?size=%s&text=%s"
          npm-manager-search-registry-host
          npm-manager-search-result-limit
          (url-encode-url search-string)))

(aio-defun npm-manager-search--fetch (search-string)
  "Search for SEARCH-STRING using NPM registry API.

Returns a promise that is fulfilled with the decoded
JSON search result."
  (-let* ((registry-url
           (npm-manager-search--make-search-url search-string))
          (((&plist :error the-error) . res-buffer)
           (aio-await (aio-url-retrieve registry-url))))
    (if the-error
        (signal the-error)
      (with-current-buffer res-buffer
        (goto-char (point-min))
        (while (looking-at "^.")
          (forward-line))
        (let* ((s (buffer-substring (point) (point-max)))
               ;; json-parse-string fails if encoding is not utf-8
               (fixed (encode-coding-string s 'utf-8 't)))
          (prog1 (json-parse-string fixed :object-type 'alist)
            (kill-buffer)))))))

(defun npm-manager-search--package-to-entry (package-object)
  "Convert npm registry PACKAGE-OBJECT to tablist entry."
  ;; see https://www.npmjs.com/package/libnpmsearch#api
  ;; and https://github.com/npm/registry/blob/master/docs/REGISTRY-API.md
  (let* ((package (map-elt package-object 'package))
         (package-name (map-elt package 'name))
         (description (map-elt package 'description ""))
         (publisher (map-nested-elt package '(publisher username) ""))
         (date (map-elt package 'date))
         (short-date (car (split-string date "T")))
         (version (map-elt package 'version ""))
         (weekly-downloads
          (file-size-human-readable (map-nested-elt package-object '(downloads weekly) 0) 'si)))
    (list
     package-object
     (vector
      package-name
      description
      publisher
      short-date
      version
      weekly-downloads))))

(defun npm-manager-search-refresh ()
  "Refresh the contents of NPM search list display."
  (interactive)
  (let* ((result (aio-wait-for (npm-manager-search--fetch npm-manager-search-string)))
         (data (map-elt result 'objects)))
    (seq-map #'npm-manager-search--package-to-entry data)))

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
  ["Filters"
   ("a" "author" "author=")
   ("m" "maintainer" "maintainer=")
   ("k" "keywords" "keywords=" :prompt "Package keywords: ")]
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
                               ("Publisher" 18 t)
                               ("Date" 12 t)
                               ("Version" 12)
                               ("Weekly DLs" 5 t)]
        tabulated-list-padding 2
        tabulated-list-entries #'npm-manager-search-refresh)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'npm-manager-search)

;;; npm-manager-search.el ends here
