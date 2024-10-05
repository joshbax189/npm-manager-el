;;; npm-manager.el --- Manage npm installations   -*- lexical-binding: t -*-

;; Copyright (C) 2024 Josh Bax

;; Author: Josh Bax

;; Keywords: languages
;; URL: https://github.com/joshbax189/npm-manager-el

;; Package-Version: 0.2.0

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

;; Display and interact with the contents of package.json.

;;; Code:

(require 'aio)
(require 'dash)
(require 'transient)
(require 'tablist)

(require 'json)
(require 'ansi-color)
(require 'filenotify)
(require 'shell)

(declare-function npm-manager-search "npm-manager-search")

(defgroup npm-manager nil
  "Interface to NPM."
  :group 'tools)

(defvar npm-manager-package-json nil "Parsed package json.")
(make-variable-buffer-local 'npm-manager-package-json)

(defvar npm-manager-package-json-watcher nil "Descriptor for package.json watcher.")
(make-variable-buffer-local 'npm-manager-package-json-watcher)

(defun npm-manager--change-handler (event)
  "Handle change in package.json.

Filenotify callback responding to change EVENT."
  (let ((event-type (nth 1 event)))

    (when (equal 'changed event-type)
        (message "package.json change detected")
        (npm-manager--parse-package-json)
        ;; This will refresh view as a side-effect
        (npm-manager--run-package-audit (current-buffer)))))

(defun npm-manager--set-package-watch (manager-buffer)
  "Reload MANAGER-BUFFER's package.json file on file change."
  (with-current-buffer manager-buffer
    (unless npm-manager-package-json-watcher
      (let ((package-json (npm-manager--get-package-json-path))
            (cb (lambda (event) (with-current-buffer manager-buffer (npm-manager--change-handler event)))))
        (setq npm-manager-package-json-watcher
              (file-notify-add-watch package-json '(change) cb))))))

(defun npm-manager--remove-package-watch ()
  "Remove file watcher from package.json.

Must be called with the `npm-manager' buffer as current."
  (when npm-manager-package-json-watcher
    (message "removed package.json watcher")
    (file-notify-rm-watch npm-manager-package-json-watcher)))

(defun npm-manager--get-package-json-path ()
  "Return the path to package.json active for the current directory.

If there is no package.json in any parent directory, then return a
new package.json in the current directory."
  (let ((default-directory (string-trim (shell-command-to-string "npm prefix"))))
    (expand-file-name "package.json")))

(defun npm-manager--get-node-modules-path ()
  "Return the path to node_modules active for the current directory.

If there is no node_modules folder in any parent directory, then return a
non-existing node_modules folder in the current directory."
  (string-trim (shell-command-to-string "npm root")))

(defun npm-manager--parse-package-json ()
  "Store parsed package.json in buffer-local variable."
  (condition-case nil
      (setq npm-manager-package-json
            (json-read-file (npm-manager--get-package-json-path)))
    (file-missing (message "Missing package.json"))))

(defvar npm-manager-audit-json nil "Parsed output of npm audit.")
(make-variable-buffer-local 'npm-manager-audit-json)

(aio-defun npm-manager--run-package-audit (manager-buffer)
  "Store output of npm audit in buffer-local variable of MANAGER-BUFFER."
  (if (not (file-exists-p (npm-manager--get-node-modules-path)))
      (message "Skipping npm audit")
    ;; else
    (let ((audit-json))
      ;; aio-await macro doesn't like to be in the let?
      (setq audit-json (aio-await (npm-manager--capture-command "npm audit --json")))
      (with-current-buffer manager-buffer
        (setq npm-manager-audit-json audit-json)
        (message "completed package audit")
        (tablist-revert)))))

(defun npm-manager-info ()
  "Run `npm info` on the package at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (name (seq-elt entry 0))
         (ver (seq-elt entry 2)))
    (npm-manager--display-command "info" "" (format "%s@%s" name ver))))

(defun npm-manager--consume-json-buffer (&optional buffer)
  "Parse JSON in BUFFER then kill it.

If BUFFER is not given, use current buffer.

Returns buffer JSON encoded as an assoc."
  (with-current-buffer (or buffer (current-buffer))
    ;; json-parse-buffer does not seem to handle trailing whitespace
    (prog1
        (json-parse-string (buffer-string)
                           :object-type 'alist)
      (kill-buffer))))

;; TODO match args to display-command
(defun npm-manager--capture-command (command-string)
  "Run npm command COMMAND-STRING and parse output as JSON.
Returns an `aio-promise' containing the parsed JSON."
  (let ((promise (aio-promise)))
    (prog1
        promise
      (make-process
       :name "npm-manager-proc"
       :buffer (generate-new-buffer (format "npm-manager-proc %s" command-string) 't)
       :command (split-string command-string)
       :noquery 't
       :stderr (get-buffer-create "*NPM Manager process errors*")
       :sentinel (lambda (proc string)
                   (cond
                    ((equal string "run\n") nil)
                    ((equal string "finished\n")
                     (aio-with-promise promise
                       (condition-case nil
                           (npm-manager--consume-json-buffer (process-buffer proc))
                         (error
                          (error "Error parsing JSON result")))))
                    ((string-prefix-p "exited abnormally" string)
                     ;; some npm commands give non-zero exit code AND produce the output we want!
                     (message "npm process errors, see error buffer")
                     (aio-with-promise promise
                       ;; first try to parse any JSON
                       (let (buffer-json)
                         (condition-case err
                             (setq buffer-json (npm-manager--consume-json-buffer (process-buffer proc)))
                             ;; cannot parse JSON so just give a generic error
                             (error
                              (message "caught %s" err)
                              (message "npm process %s" string)
                              (message "see buffer %s" "*NPM Manager process errors*")
                              (error string)))
                         ;; parsed JSON may contain an error prop
                         (if-let ((the-error (map-elt buffer-json 'error)))
                             (error "%s" the-error)
                           ;; result
                           buffer-json))))
                    ('t
                     (message "npm process %s" string))))))))

(defun npm-manager--display-command (command flags args)
  "Run a command and display output in a new buffer.

Command will be like `npm COMMAND FLAGS ARGS' where:
  COMMAND is a string
  FLAGS is a string, and
  ARGS is a string.

Returns an `aio-promise' that is fulfilled with the output buffer."
  (let ((promise (aio-promise))
        (proc-buffer (generate-new-buffer (format "*npm %s %s*" command args))))
    (prog1
        promise
      (setq flags (or flags ""))
      (setq args (or args ""))
      (make-process
       :name "npm-manager-proc"
       :buffer proc-buffer
       :command (append (list "npm" command) (split-string flags) '("--color" "always") (split-string args))
       :noquery 't
       :filter (lambda (proc string)
                 (when (buffer-live-p (process-buffer proc))
                   (with-current-buffer (process-buffer proc)
                     (let ((moving (= (point) (process-mark proc))))
                       (save-excursion
                         ;; Insert the text, advancing the process marker.
                         (goto-char (process-mark proc))
                         (insert (ansi-color-apply string))
                         (set-marker (process-mark proc) (point)))
                       (if moving (goto-char (process-mark proc)))))))
       :sentinel (lambda (proc string)
                   (cond
                    ((equal string "run\n") nil)
                    ((or (equal string "finished\n")
                         ;; display result in every case
                         (string-prefix-p "exited abnormally" string))
                     (with-current-buffer (process-buffer proc)
                       (shell-mode)
                       (view-mode)
                       (pop-to-buffer (current-buffer)))
                     (aio-with-promise promise
                       (process-buffer proc)))
                    ('t (message string))))))))

(defun npm-manager--make-entry (dependencies package-name)
  "Create tablist entry given PACKAGE-NAME symbol.

DEPENDENCIES is an alist of package name to version string."
  (-let* (((dependency requested-version) (npm-manager--read-dep-type package-name))
          (name (symbol-name package-name))
          (propertized-name (if (equal dependency "req")
                                (propertize name 'font-lock-face 'bold)
                              name))
          (installed-version (map-elt dependencies package-name))
          (vulnerabilities (npm-manager--read-vuln package-name)))
   (vector
    propertized-name
    dependency
    requested-version
    (or installed-version "-")
    vulnerabilities)))

(defun npm-manager-refresh ()
  "Refresh the contents of NPM manager display."
  (interactive)
  (unless npm-manager-package-json
    (npm-manager--parse-package-json))
  ;; trigger async call but don't block
  (unless npm-manager-audit-json
    (npm-manager--run-package-audit (current-buffer)))
  (message "%s %s" (map-elt npm-manager-package-json 'name) (map-elt npm-manager-package-json 'version))
  (let* ((installed-packages (aio-wait-for (npm-manager--list-installed-versions)))
         (package-names (if installed-packages
                            (map-keys installed-packages)
                          (npm-manager--read-packages))))
    (--map (list it (npm-manager--make-entry installed-packages it))
           package-names)))

(aio-defun npm-manager--list-installed-versions ()
  "Return an alist of installed dependency packages and their versions.
Example output:
((camelcase . \"8.0.0\")
 (change-case . \"5.4.4\"))

When no packages are installed, or package listing results in an error,
returns nil."
  ;; For some reason ignore-errors returns the error object, not nil
  (condition-case nil
      (let* ((installed-packages (aio-await (npm-manager--capture-command "npm list --json")))
             (dependencies (map-elt installed-packages 'dependencies))
             (dependencies (map-into dependencies 'alist))
             ;; Remove dependencies marked "extraneous".
             ;; These are the result of npm i --no-save for example.
             (true-dependencies (--remove
                                 (map-elt (cdr it) 'extraneous)
                                 dependencies)))
        (map-apply
         (lambda (k v) (cons k (map-elt v 'version)))
         true-dependencies))
    (error '())))

(defun npm-manager--read-dep-type (package-name)
  "Look up dependency type (dev, peer, etc) of symbol PACKAGE-NAME.
Returns a list: (type requested-version)."
  (let ((core-deps     (map-nested-elt npm-manager-package-json `(dependencies ,package-name)))
        (dev-deps      (map-nested-elt npm-manager-package-json `(devDependencies ,package-name)))
        (peer-deps     (map-nested-elt npm-manager-package-json `(peerDependencies ,package-name)))
        (optional-deps (map-nested-elt npm-manager-package-json `(optionalDependencies ,package-name)))
        (bundle-deps   (map-nested-elt npm-manager-package-json `(bundleDependencies ,package-name))))

    (or
     (and core-deps     `("req" ,core-deps))
     (and dev-deps      `("dev" ,dev-deps))
     (and peer-deps     `("peer" ,peer-deps))
     (and optional-deps `("opt" ,optional-deps))
     (and bundle-deps   `("bundle" ,bundle-deps))
     '("" ""))))

(defun npm-manager--read-packages ()
  "List packages from package.json instead of node_modules.
Use when `npm-manager--list-installed-versions' doesn't work."
  (unless npm-manager-package-json (error "Missing package.json"))

  (map-keys
   (append
    (map-elt npm-manager-package-json 'dependencies) ;; list of cons cells (package-symbol . "version")
    (map-elt npm-manager-package-json 'devDependencies)
    (map-elt npm-manager-package-json 'peerDependencies)
    (map-elt npm-manager-package-json 'optionalDependencies)
    (map-elt npm-manager-package-json 'bundleDependencies))))

(defun npm-manager--read-vuln (package-name)
  "Get vulnerability reports for PACKAGE-NAME symbol.
Returns a string high/medium/low or empty."
  (let ((severity (map-nested-elt npm-manager-audit-json `(vulnerabilities ,package-name severity) "")))
    (cond
     ((equal severity "high") (propertize severity 'font-lock-face 'error))
     ((equal severity "medium") (propertize severity 'font-lock-face 'warning))
     ('t severity))))

(aio-defun npm-manager-uninstall ()
  "Uninstall package at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (name (seq-elt entry 0))
         (npm-buffer (current-buffer)))
    (aio-await (npm-manager--display-command "uninstall" "" name))
    (with-current-buffer npm-buffer (revert-buffer))))

(aio-defun npm-manager-install-types-package (base-package-name)
  "Install the types package for BASE-PACKAGE-NAME."
  (interactive (list (completing-read
                 "@types/"
                 (list (or (string-remove-prefix "@types/" (seq-elt (tabulated-list-get-entry) 0))
                           "")))))
  (aio-await (npm-manager--display-command "i" "-D" (format "@types/%s" base-package-name))))

(aio-defun npm-manager-install-packages ()
  "Runs npm install."
  (interactive)
 (let ((npm-buffer (current-buffer)))
   (aio-await (npm-manager--display-command "i" "" ""))
   (with-current-buffer npm-buffer
     (when (equal major-mode 'npm-manager-mode)
       (revert-buffer)))))

(aio-defun npm-manager-change-package-type (new-type)
 "Change dependency type of package."
 (interactive (list
               (completing-read
                "Install as: " (list "Prod" "Dev" "Optional"))))
 (let ((package-name (seq-elt (tabulated-list-get-entry) 0))
       (flag (format "-%s" (seq-take new-type 1))))
   (aio-await (npm-manager--display-command "i" flag package-name))))

(aio-defun npm-manager-display-audit ()
 "Run npm audit and show result."
 (interactive)
 (message "Run: npm audit")
 (aio-await (npm-manager--display-command "audit" "" "")))

;;;###autoload
(defun npm-manager ()
  "Start npm manager interface in the directory."
  (interactive)
  (let* ((npm-buffer (format "NPM %s" default-directory))
         (buffer-exists (get-buffer npm-buffer)))
    (pop-to-buffer npm-buffer)
    (npm-manager--parse-package-json)
    (unless buffer-exists
      (npm-manager-mode)
      (npm-manager--set-package-watch (current-buffer))
      (add-hook 'kill-buffer-hook #'npm-manager--remove-package-watch))
    (tablist-revert)))

(defun npm-manager-unload-function ()
  "Cleanup mode hooks."
  (remove-hook 'kill-buffer-hook #'npm-manager--remove-package-watch))

(defvar npm-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "I"         #'npm-manager-change-package-type)
    (define-key map "A"         #'npm-manager-display-audit)
    (define-key map "S"         #'npm-manager-search)
    (define-key map "D"         #'npm-manager-uninstall)
    (define-key map (kbd "RET") #'npm-manager-info)
    map)
  "Keymap for `npm-manager-search-mode'.")

(define-derived-mode npm-manager-mode tabulated-list-mode "NPM Manager"
  "NPM manager major mode."
  (setq tabulated-list-format [("Package" 48 t)
                               ("Type" 6 t)
                               ("Requested" 12 t)
                               ("Installed" 12 t)
                               ("Messages" 32 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Type" . t)
        tabulated-list-entries #'npm-manager-refresh)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'npm-manager)

;;; npm-manager.el ends here
