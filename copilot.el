;;; copilot.el --- Emacs interface for GitHub Copilot CLI chat  -*- lexical-binding: t; -*-

;; Author: Auto Generated
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai
;; URL: https://github.com/example/copilot-emacs

;;; Commentary:
;; Minimal implementation of the user stories in README.org.
;; Entry point: `copilot/new'.  See README for behaviour.
;;
;; Notes:
;; - Streaming is implemented via a process filter that appends to the
;;   current AI block.
;; - Session id is heuristically extracted from process output using
;;   regexp `copilot--session-id-regexp'.  If none is found we fabricate
;;   one (buffer-local) so resume still works (albeit without a real
;;   server session).
;; - Safeguards: variable `copilot-cli-safeguard-function' can validate
;;   or transform the prompt before it is sent.  Should signal `user-error'
;;   to abort.

;;; Code:
(require 'subr-x)
(require 'org)

(defgroup copilot-cli nil
  "Copilot CLI Emacs integration."
  :group 'tools)

(defcustom copilot-cli-command "copilot"
  "Command used to invoke the Copilot CLI."
  :type 'string)

(defcustom copilot-cli-model "gpt-5"
  "Default model."
  :type 'string)

(defcustom copilot-cli-deny-tools '("shell(git:*)")
  "List supplied to repeated --deny-tool arguments."
  :type '(repeat string))

(defcustom copilot-cli-add-dirs nil
  "List of directories for repeated --add-dir arguments."
  :type '(repeat directory))

(defcustom copilot-cli-extra-args nil
  "Extra raw arguments appended to the command line."
  :type '(repeat string))

(defcustom copilot-cli-safeguard-function nil
  "Function called with prompt. Return prompt (maybe modified) or signal to abort."
  :type '(choice (const :tag "None" nil) function))

(defvar-local copilot--session-id nil)
(defconst copilot--session-id-regexp "Session ID: *\([A-Za-z0-9_-]+\)" )
(defconst copilot--ai-begin "#+begin_ai\n")
(defconst copilot--ai-end   "#+end_ai\n")

(defun copilot--project-root ()
  (cond
   ((featurep 'projectile)
    (or (ignore-errors (projectile-project-root)) default-directory))
   (t default-directory)))

(defun copilot--truncate-words (s n)
  (let* ((words (split-string s "[\n\r\t ]+" t))
         (head (seq-take words n)))
    (string-join head " ")))

(defun copilot--buffer-name (prompt)
  (format "*Copilot [%s] - %s*"
          (file-name-nondirectory (directory-file-name (copilot--project-root)))
          (copilot--truncate-words prompt 10)))

(defun copilot--current-session-id ()
  (or copilot--session-id
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (goto-char (point-min))
          (org-entry-get (point) "copilot-session-id")))
      nil))

(defun copilot--set-session-id (id)
  (setq copilot--session-id id)
  (org-set-property "copilot-session-id" id))

(defun copilot--insert-new-user-prompt (prompt)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (insert (format "* User\n%s\n" prompt)))

(defun copilot--ensure-ai-block ()
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (insert copilot--ai-begin)
  (let ((start (point)))
    (insert "")
    (put-text-property start start 'copilot-ai-start t))
  (insert copilot--ai-end)
  (forward-line -1))

(defun copilot--ai-block-region ()
  (save-excursion
    (goto-char (point-max))
    (when (search-backward copilot--ai-begin nil t)
      (let ((beg (match-end 0)))
        (when (search-forward copilot--ai-end nil t)
          (cons beg (match-beginning 0)))))))

(defun copilot--append-to-ai-block (text)
  (let ((reg (copilot--ai-block-region)))
    (when reg
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (cdr reg))
          (insert text))))))

(defun copilot--finalize-ai-block ()
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "# Done\n"))))

(defun copilot--build-command (prompt &optional resume-id)
  (let ((base (list copilot-cli-command))
        (deny (apply #'append (mapcar (lambda (d) (list "--deny-tool" d)) copilot-cli-deny-tools)))
        (dirs (apply #'append (mapcar (lambda (d) (list "--add-dir" d)) copilot-cli-add-dirs)))
        (extra copilot-cli-extra-args))
    (append base
            (if resume-id
              (list "-p" (format "\"%s\"" prompt) "--model" copilot-cli-model "--allow-all-tools" "--resume" resume-id)
              (list "-p" (format "\"%s\"" prompt) "--model" copilot-cli-model "--allow-all-tools"))
            deny dirs extra)))

(copilot--build-command "hello" "123")

(defun copilot--process-filter (proc chunk)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (unless copilot--session-id
          (when (string-match copilot--session-id-regexp chunk)
            (copilot--set-session-id (match-string 1 chunk))))
        (copilot--append-to-ai-block chunk)))))

(defun copilot--process-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (copilot--finalize-ai-block)))))

(defun copilot--start-process (prompt resume-id)
  (let* ((default-directory (copilot--project-root))
         (cmd (copilot--build-command prompt resume-id))
         (buf (current-buffer))
         (proc (apply #'start-process "copilot-cli" buf cmd)))
    (message (format "Calling copilot like =%s=" cmd))
    (set-process-filter proc #'copilot--process-filter)
    (set-process-sentinel proc #'copilot--process-sentinel)
    proc))

(defun copilot--prepare-buffer (prompt)
  (let* ((buf (generate-new-buffer (copilot--buffer-name prompt)))
         (ts (format-time-string "%Y-%m-%d %H:%M")))
    (with-current-buffer buf
      (org-mode)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (insert (format "#+title: %s\n" ts))
        (insert "* Chat\n")
        (copilot--insert-new-user-prompt prompt)
        (copilot--ensure-ai-block))
      buf)))

(defun copilot--resume-buffer-p ()
  (and (derived-mode-p 'org-mode)
       (copilot--current-session-id)))

;;;###autoload
(defun copilot/new (prompt)
  "Start or continue a Copilot chat session with PROMPT.
When called from an existing Copilot buffer (has session id), the
session is resumed via --resume.  Otherwise a new chat is started."
  (interactive (list (read-string "Copilot prompt: ")))
  (when copilot-cli-safeguard-function
    (setq prompt (funcall copilot-cli-safeguard-function prompt)))
  (if (copilot--resume-buffer-p)
      (let* ((inhibit-read-only t)
             (sid (copilot--current-session-id)))
        (goto-char (point-max))
        (copilot--insert-new-user-prompt prompt)
        (copilot--ensure-ai-block)
        (copilot--start-process prompt sid))
    (let ((buf (copilot--prepare-buffer prompt)))
      (switch-to-buffer buf)
      (copilot--start-process prompt nil)))
  (message "Copilot: started"))

(provide 'copilot)
;;; copilot.el ends here

