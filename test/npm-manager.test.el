;; -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'ert-async)
(require 'npm-manager)
(require 'aio)
(require 'map)

(defun npm-manager-test--init-test-folder (folder-name packages)
  "Installs PACKAGES in FOLDER-NAME."
  (unless (file-directory-p folder-name)
    (make-directory folder-name))
  (let ((default-directory (concat default-directory folder-name)))
    ;; (shell-command (format "npm i %s" (string-join packages " ")))
    (npm-manager--capture-command (format "npm i --json %s" (string-join packages " ")))))

(ert-deftest npm-manager--consume-json-buffer/test ()
  "Parse JSON buffers."
  ;; empty
  (with-temp-buffer
    (should-error (npm-manager--consume-json-buffer)))

  ;; null object
  (with-temp-buffer
    (insert "{}")
    (should (null (npm-manager--consume-json-buffer))))

  ;; parse error
  (with-temp-buffer
    (insert "foo {")
    (should-error (npm-manager--consume-json-buffer)))

  ;; should kill buffer
  (let ((test-buffer (generate-new-buffer "foo")))
   (with-current-buffer test-buffer
     (insert "{}")
     (npm-manager--consume-json-buffer))
   (should (not (buffer-live-p test-buffer)))))

;;;; npm-manager--change-handler
;;;; npm-manager--set-package-watch
;;;; npm-manager--remove-package-watch
;; better to have integration tests for these:
(ert-deftest-async npm-manager/test-file-watch (done)
  "External modifications to package.json should be reflected."
  (kill-matching-buffers-no-ask "NPM .*/foo/")
  (aio-listen
   (npm-manager-test--init-test-folder "foo" '("color" "jose"))
   (lambda (_x)
     (let ((default-directory (expand-file-name "foo/"))
           saved-watcher)
       (npm-manager)
       (aio-wait-for (aio-sleep 3))
       ;; package watch is set
       (should npm-manager-package-json-watcher)
       ;; external change to file triggers a change
       (goto-char (point-min))
       (if (re-search-forward "rimraf" nil 't)
           ;; if it exists, delete
           (progn
             (shell-command "npm uninstall rimraf")
             (should-not (re-search-forward "rimraf" nil 't)))
         ;; otherwise install
         (shell-command "npm i rimraf")
         (should (re-search-forward "rimraf" nil 't)))

       ;; watch should be removed on quit
       (setq saved-watcher npm-manager-package-json-watcher)
       (should saved-watcher) ;; sanity check
       (kill-current-buffer)

       (should-not (gethash saved-watcher file-notify-descriptors))
       (funcall done)))))

;;;; npm-manager--get-package-json-path
(ert-deftest npm-manager--get-package-json-path/test ()
  "Should return path to package.json for the current directory."
  (let ((default-directory (expand-file-name "./basic_project")))
    (should (equal (expand-file-name "./package.json")
                   (npm-manager--get-package-json-path))))
  ;; also test in a subdirectory
  (let ((expected-name (expand-file-name "./basic_project/package.json"))
        (default-directory (expand-file-name "./basic_project/node_modules")))
    (should (equal expected-name
                   (npm-manager--get-package-json-path)))))

(ert-deftest npm-manager--get-package-json-path/test-empty ()
  "Tests behaviour in empty project."
  (let ((default-directory (expand-file-name "./empty_project")))
    (should (equal (expand-file-name "./package.json")
             (npm-manager--get-package-json-path)))))

(ert-deftest npm-manager--get-node-modules-path/test ()
  "List installed packages in JSON."
  (npm-manager-test--init-test-folder "foo" '("color" "jose"))
  ;; with normal setup
  (let* ((default-directory (concat default-directory "foo/"))
         (result (npm-manager--get-node-modules-path)))
    (should (equal result
                   (concat default-directory "node_modules"))))
  ;; with no node_modules
  (should (equal (npm-manager--get-node-modules-path)
                 (concat default-directory "node_modules"))))

;;;; npm-manager--parse-package-json
(ert-deftest npm-manager--parse-package-json/test ()
  "Tests default case."
  ;; calls package-json-path, which requires default-directory
  (let ((default-directory (expand-file-name "basic_project")))
    ;; sets package-json in current buffer
    (with-temp-buffer
      (npm-manager--parse-package-json)
      (should npm-manager-package-json)
      (should (equal "basic_project"
                     (map-elt npm-manager-package-json 'name))))))

(ert-deftest npm-manager--parse-package-json/test-empty ()
  "When package.json does not exist, it should not produce anything."
  (let ((package-json-path (npm-manager--get-package-json-path)))
    (should (not (file-exists-p package-json-path)))
    (with-temp-buffer
      (npm-manager--parse-package-json)
      (should (not npm-manager-package-json)))))

;;;; npm-manager--run-package-audit
(ert-deftest-async npm-manager--run-package-audit/test-async (done)
  "Tests default behaviour."
  (let ((default-directory (expand-file-name "basic_project")))
    (with-mock
      ;; Must refresh tablist
      (mock (tablist-revert))
      (with-temp-buffer
        (aio-wait-for (npm-manager--run-package-audit (current-buffer)))
        ;; output in npm-manager-audit-json
        (should npm-manager-audit-json)
        (funcall done)))))

(ert-deftest-async npm-manager--run-package-audit/test-empty-async (done-1)
  "Tests behaviour when no node_modules."
  (let ((default-directory (expand-file-name "empty_project")))
   (with-temp-buffer
     (aio-wait-for (npm-manager--run-package-audit (current-buffer)))
     ;; output in npm-manager-audit-json
     (should (not npm-manager-audit-json))
     (funcall done-1))))

;;;; npm-manager-info
(ert-deftest npm-manager-info/test ()
  "Should call display-command with npm info package@version."
  (with-temp-buffer
    (with-mock
     (mock (tabulated-list-get-entry) => '("foo" nil "0.1"))
     (mock (npm-manager--display-command "info" "" "foo@0.1"))
     (npm-manager-info))))

;;;; npm-manager--capture-command
(ert-deftest-async npm-manager--capture-command/test (done)
  "Tests normal behaviour."
  (should (aio-wait-for (npm-manager--capture-command "npm version --json")))
  (funcall done))

(ert-deftest-async npm-manager--capture-command/test-invalid-json (done)
  "When output is not valid json, should error."
  (should-error (aio-wait-for (npm-manager--capture-command "npm version")))
  (funcall done))

(ert-deftest-async npm-manager--capture-command/test-error (done)
  "When npm produces a JSON formatted error, should error."
  (let ((default-directory (expand-file-name "empty_project/")))
    ;; output is { error: foo } and non-zero code
    (should-error (aio-wait-for (npm-manager--capture-command "npm show --json")))
    (funcall done)))

(ert-deftest-async npm-manager--capture-command/test-error-2 (done)
  "When npm produces an error and no JSON output, should error."
  (let ((default-directory (expand-file-name "empty_project/")))
    (should-error (aio-wait-for (npm-manager--capture-command "npm eugh --json")))
    (funcall done)))

(ert-deftest-async npm-manager--capture-command/test-error-output (done)
  "When npm has non-zero exit and output, should return output."
  (let ((default-directory (expand-file-name "bad_audit_project/")))
    (should (map-elt (aio-wait-for (npm-manager--capture-command "npm audit --json"))
                     'vulnerabilities))
    (funcall done)))

;; TODO
;;;; npm-manager--display-command

;;;; npm-manager--make-entry
(ert-deftest npm-manager--make-entry/test ()
  "Tests basic behavior."
  (with-mock
    (stub npm-manager--read-dep-type => '("req" "0.0.1"))
    (stub npm-manager--read-vuln => "medium")
    (let ((result (npm-manager--make-entry '((foo . "1.0.0")) 'foo)))
     (should (equal
              (vector "foo" "req" "0.0.1" "1.0.0" "medium")
              result))
     (should (equal
              'bold
              (get-text-property 0 'font-lock-face (seq-elt result 0)))))))

(ert-deftest npm-manager--make-entry/test-empty-deps ()
  "Tests no dependencies."
  (with-mock
    (stub npm-manager--read-dep-type => '("req" "0.0.1"))
    (stub npm-manager--read-vuln => "medium")
    (let ((result (npm-manager--make-entry nil 'foo)))
     (should (equal
              (vector "foo" "req" "0.0.1" "-" "medium")
              result)))))

;;;; npm-manager-refresh
(ert-deftest npm-manager-refresh/test-initial ()
  "Tests behaviour in a fresh buffer."
  (with-mock
    ;; should call package.json and audit
    (mock (npm-manager--parse-package-json))
    (mock (npm-manager--run-package-audit *))
    ;; should call list-installed-versions
    (mock (npm-manager--list-installed-versions) => (aio-sleep 1
                                                               '((foo . 1) (bar . 2))))
    (with-temp-buffer
      (npm-manager-refresh))))

(ert-deftest npm-manager-refresh/test-no-install ()
  "Tests behaviour with no node_modules."
  (with-mock
    (stub npm-manager--list-installed-versions => (aio-sleep 1 nil))
    ;; if nil, should call read-packages
    (mock (npm-manager--read-packages) => '(foo bar))
    (with-temp-buffer
      (setq npm-manager-audit-json '(()))
      (setq npm-manager-package-json '(()))
      (npm-manager-refresh))))

;;;; npm-manager--read-dep-type
(ert-deftest npm-manager--read-dep-type/test-nil ()
  "When there is no package.json should return (\"\" \"\")."
  (with-temp-buffer
    (should (equal '("" "")
                   (npm-manager--read-dep-type "foo")))))

;;;; npm-manager--read-packages
(ert-deftest npm-manager--read-packages/test-nil ()
  "Should error when no pacakge.json."
  (with-temp-buffer
    (should-error (npm-manager--read-packages))))

(ert-deftest-async npm-manager--read-packages/test-match-list-output (done)
  "Should match output of list-installed-versions."
  (let* ((default-directory (expand-file-name "./basic_project"))
         (list-result (aio-wait-for (npm-manager--list-installed-versions))))
    (with-temp-buffer
      (npm-manager--parse-package-json)
      (should (equal
               (map-keys list-result)
               (npm-manager--read-packages)))))
  (funcall done))

;;;; npm-manager--read-vuln
;;;; npm-manager-tree

;;;; npm-manager--list-installed-versions
(ert-deftest-async npm-manager--list-installed-versions/test (done)
  "List installed packages in JSON."
  (let* ((default-directory (expand-file-name "./basic_project"))
         (result (aio-wait-for (npm-manager--list-installed-versions))))
    (should (equal result
                   '((camelcase . "8.0.0") (change-case . "5.4.4")))))
  (funcall done))

(ert-deftest-async npm-manager--list-installed-versions/test-extraneous (done)
  "Extraneous packages should not be listed."
  (with-mock
    (stub npm-manager--capture-command => (aio-sleep 0.1 '((dependencies . ((foo . ((extraneous . t))))))))
    (let ((result (aio-wait-for (npm-manager--list-installed-versions))))
      (should (equal (map-keys result) nil))))
  (funcall done))

(ert-deftest-async npm-manager--list-installed-versions/test-empty (done)
  "Should return nil when no package.json."
  (let* ((default-directory (expand-file-name "./empty_project"))
         (result (aio-wait-for (npm-manager--list-installed-versions))))
    (should (not result)))
  (funcall done))

(ert-deftest-async npm-manager--list-installed-versions/test-missing (done)
  "Should return nil when no node_modules."
  (let* ((default-directory (expand-file-name "./missing_install_project"))
         (result (aio-wait-for (npm-manager--list-installed-versions))))
    (should (not result)))
  (funcall done))

;;;; npm-manager
(ert-deftest-async npm-manager/test-basic-display (done)
  "Can display packages."
  (kill-matching-buffers-no-ask "NPM .*/foo/")
  ;; TODO why doesn't it work when working with an existing buffer?
  (aio-listen
   (npm-manager-test--init-test-folder "foo" '("color" "jose"))
   (lambda (_x)
     (let ((default-directory (expand-file-name "foo/")))
       (npm-manager)
       (aio-wait-for (aio-sleep 3))
       ;; check first line
       (should (equal (string-clean-whitespace (buffer-substring-no-properties (point-min) (pos-eol)))
                      "color req ^4.2.3 4.2.3"))
       (funcall done)))))

;; works with no package-lock
;; works with no node_modules

;;;; npm-manager-unload-function

;;;; npm-manager-uninstall
(ert-deftest-async npm-manager/test-uninstall (done)
  "Can uninstall packages."
  (aio-wait-for (npm-manager-test--init-test-folder "foo" '("color" "jose")))
  (let ((default-directory (concat default-directory "foo/")))
    (npm-manager)
    (aio-wait-for (aio-sleep 3))
    (beginning-of-buffer)
    (should (equal (seq-elt (tabulated-list-get-entry) 0) "color"))
    ;; (aio-wait-for (npm-manager-uninstall))
    ;; (should (not (equal (seq-elt (tabulated-list-get-entry) 0) "color")))
    ))

;; ./npm-manager.el:307:(aio-defun npm-manager-install-types-package (base-package-name)
;; ./npm-manager.el:318:(aio-defun npm-manager-install-packages ()
;; ./npm-manager.el:326:(aio-defun npm-manager-change-package-type (new-type)

;;;; npm-manager-mode
