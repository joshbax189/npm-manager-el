;;; npm-manager-search.el --- Emacs search npm package registry   -*- lexical-binding: t -*-

;; Copyright (C) 2024 Josh Bax

;; Author: Josh Bax

;; Keywords: languages
;; URL: https://github.com/joshbax189/npm-manager-el

;; Package-Requires: (aio dash json tablist transient url)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Search NPM registry for packages.

;;; Code:

(require 'aio)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'transient)
(require 'url)

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

(defun npm-manager-search-fetch (search-string)
  "Call search API."
  (-let [(callback . promise) (aio-make-callback :once 't)]
    (url-retrieve (format "%s/-/v1/search?size=%s&text=%s" npm-manager-search-registry-host npm-manager-search-result-limit search-string)
                  (lambda (&rest _1)
                    (while (looking-at "^.") (delete-line))
                    (apply callback (json-parse-buffer :object-type 'alist) nil)))
    promise))

(defun npm-manager-search--format-score (score-num)
  "Format SCORE-NUM for display in tablist."
  (seq-take (number-to-string score-num) 4))

(defun npm-manager-search-refresh ()
  "Refresh the contents of NPM search list display."
  (interactive)
  ;; see https://www.npmjs.com/package/libnpmsearch#api
  ;; and https://github.com/npm/registry/blob/master/docs/REGISTRY-API.md
  (let* ((result (car (aio-wait-for (npm-manager-search-fetch npm-manager-search-string))))
         (data (map-elt result 'objects)))
    (--map (list it
                 (let ((package (map-elt it 'package))
                       (score (map-elt it 'score)))
                   (let-alist package
                     (vector .name
                             (or .description "")
                             (or (map-elt .author 'name)
                                 "")
                             (car (string-split .date "T"))
                             .version
                             (npm-manager-search--format-score (map-nested-elt score '(detail quality)))
                             (npm-manager-search--format-score (map-nested-elt score '(detail popularity)))
                             (npm-manager-search--format-score (map-nested-elt score '(detail maintenance)))
                             ))))
           data)))

(defun npm-manager-search-info ()
  "Run `npm info` on the package at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (name (seq-elt entry 0))
         (ver (seq-elt entry 4)))
    (npm-manager--display-command "info" "" (format "%s@%s" name ver))))

(defun npm-manager-search-install (package-directory)
  "Install package at point into PACKAGE-DIRECTORY."
  (interactive "D")
  (let* ((npm-buffer (current-buffer))
         (entry (tabulated-list-get-entry))
         (package-name (seq-elt entry 0)))
    (aio-await (npm-manager--display-command "i" "-D" package-name package-directory))
    (with-current-buffer npm-buffer
      (when (equal major-mode 'npm-manager-mode) (revert-buffer)))))

;;;###autoload
(defun npm-manager-search ()
  "Search npm packages."
  (interactive)
  ;; TODO this doesn't appear to work
  (when (transient-prefix-object) (transient-reset))
  (setq npm-manager-search-call-directory "")
  (npm-manager-search-again))

(transient-define-prefix npm-manager-search-again ()
  "Search npm packages inheriting previous search state."
  [("a" "author" "author=")
   ("m" "maintainer" "maintainer=")
   ("@" "scope" "scope=" :prompt "Package scope: ")
   ("k" "keywords" "keywords=" :prompt "Package keywords: ")
   ("u" "Exclude packages whose version is < 1.0.0" "not:unstable")
   ("i" "Exclude packages that are insecure or have vulnerable dependencies" "not:insecure")
   ("U" "Show/filter packages whose version is < 1.0.0)" "is:unstable")
   ("I" "Show/filter packages that are insecure or have vulnerable dependencies" "is:insecure" )
   ("x" "Don't boost exact matches" "boost-exact:false")
   ]
  [("s" "Enter search text" npm-manager-search--search-suffix)]
  )

(defvar npm-manager-search-user-text "" "Last input.")
(make-variable-buffer-local 'npm-manager-search-user-text)

(defun npm-manager-search--search-suffix (search-input)
  (interactive (list (completing-read "search: "
                                      (list npm-manager-search-user-text))))
  (transient-set)
  (let* ((data (transient-args transient-current-command))
         (clean-data (--map (string-replace "=" ":" it) data))
         (full-search (string-join (cons search-input clean-data) " ")))
    (npm-manager-search-text full-search (not (string-empty-p npm-manager-search-string)) search-input)))

(defun npm-manager-search-text (search-string &optional reuse-buffer original-input)
  "Search for an NPM package using SEARCH-STRING."
  (interactive "M")
  (if reuse-buffer
      (rename-buffer (format "NPM search: %s" search-string))
    (pop-to-buffer (format "NPM search: %s" search-string))
    (npm-manager-search-mode))
  (when original-input (setq npm-manager-search-user-text original-input))
  (setq npm-manager-search-string search-string)
  (tablist-revert))

;; TODO the keymap appears incorrect in the mode help.
;; e.g. < > and s
(defvar npm-manager-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "I"         #'npm-manager-search-install)
    (define-key map "S"         #'npm-manager-search-again)
    (define-key map (kbd "RET") #'npm-manager-search-info))
  "Keymap for `npm-manager-search-mode'.")

(define-derived-mode npm-manager-search-mode tabulated-list-mode "NPM Search"
  "NPM search result display major mode."
  (setq tabulated-list-format [("Name" 24 t)
                               ("Description" 48 t)
                               ("Author" 18 t)
                               ("Date" 12 t)
                               ("Version" 12)
                               ("Qual" 5)
                               ("Pop" 5)
                               ("Maint" 5)]
        tabulated-list-padding 2
        tabulated-list-entries #'npm-manager-search-refresh)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'npm-manager-search)

;;; npm-manager-search.el ends here
