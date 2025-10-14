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
;; - Session id is extracted from the name of the single log file created
;;   in a temporary --log-dir for the process. After process exit we look
;;   for *.log and record '# Session: <id>' near top of buffer.

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

;; Session id is discovered from the single log file in a temp --log-dir.
;; Stored in buffer as a global file property '#+property: copilot-session-id <id>'.
(defvar-local copilot--log-dir nil)
(defconst copilot--ai-begin "#+begin_ai\n")
(defconst copilot--ai-end   "#+end_ai\n")
(defcustom copilot-side-window-width 0.25
  "Width for the Copilot side window.
If a float, interpreted as fraction of the current frame width.
If an integer, interpreted as number of columns.
Set to nil to disable special side-window display."
  :type '(choice (float :tag "Fraction of frame width")
                 (integer :tag "Columns")
                 (const :tag "Disabled" nil)))

(defun copilot--display-buffer (buf)
  "Display BUF in a right side window honoring `copilot-side-window-width'.
Returns the window used."
  (if (null copilot-side-window-width)
      (progn (switch-to-buffer buf)
	     (get-buffer-window buf))
    (unless (get-buffer-window buf)
      (let* ((fw (frame-width))
             (cols (if (floatp copilot-side-window-width)
                       (max 20 (truncate (* fw copilot-side-window-width)))
                     copilot-side-window-width)))
	(display-buffer-in-side-window
	 buf `((side . right)
               (slot . -1)
               (window-width . ,cols)
               (preserve-size . (t . nil))))))))

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

(defun copilot--current-session-id (buffer)
  (with-current-buffer buffer
    (when (derived-mode-p 'org-mode)
	(org-entry-get (point) "copilot--session-id"))))

(defun copilot--set-session-id (buffer id)
  (with-current-buffer buffer
    (org-set-property "copilot--session-id" id)))

(defun copilot--insert-new-user-prompt (buf prompt)
  (with-current-buffer buf
      (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "%s\n" prompt))))

(defun copilot--ensure-ai-block (buf)
  (with-current-buffer buf
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert copilot--ai-begin)
    (let ((start (point)))
      (insert "")
      (put-text-property start start 'copilot-ai-start t))
    (insert copilot--ai-end)
    (forward-line -1)))

(defun copilot--ai-block-region (buf)
  (save-excursion
    (with-current-buffer buf
      (goto-char (point-max))
      (when (search-backward copilot--ai-begin nil t)
	(let ((beg (match-end 0)))
          (when (search-forward copilot--ai-end nil t)
            (cons beg (match-beginning 0))))))))

(defun copilot--append-to-ai-block (buf text)
  (let ((reg (copilot--ai-block-region buf)))
    (when reg
      (let ((inhibit-read-only t))
        (save-excursion
	  (with-current-buffer buf
	    (goto-char (cdr reg))
	    (insert text)))))))

(defun copilot--finalize-ai-block (buf)
  (let ((inhibit-read-only t))
    (save-excursion
      (with-current-buffer buf
	(goto-char (point-max))
	(unless (bolp) (insert "\n"))
	(insert "# End\n")))))

(defun copilot--build-command (prompt &optional resume-id log-dir)
  (let ((base (list copilot-cli-command))
        (deny (apply #'append (mapcar (lambda (d) (list "--deny-tool" d)) copilot-cli-deny-tools)))
        (dirs (apply #'append (mapcar (lambda (d) (list "--add-dir" d)) copilot-cli-add-dirs)))
        (extra copilot-cli-extra-args))
    (append base
            (list "--log-dir" log-dir)
	    (list "-p" prompt "--model" copilot-cli-model "--allow-all-tools")
            (if resume-id (list "--resume" resume-id))
            deny dirs extra)))

(defun copilot--process-filter (proc chunk)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (copilot--append-to-ai-block (current-buffer) chunk)))))

(defun copilot--process-sentinel (proc event)
  (let ((buf (process-buffer proc)))
    (when (and (buffer-live-p buf)
	     (memq (process-status proc) '(exit signal))))
    	(copilot--finalize-ai-block buf)))

(defun copilot--make-temp-log-dir ()
  (make-temp-file "copilot-log-" t))

(defun copilot--discover-session-id (log-dir)
  (let* ((files (directory-files log-dir nil "\\.log$")))
    (when (= (length files) 1)
      (file-name-base (car files)))))

(defun copilot--start-process (prompt buf resume-id)
  (let* ((default-directory (copilot--project-root))
         (log-dir (or copilot--log-dir
                      (setq copilot--log-dir (copilot--make-temp-log-dir))))
         (cmd (copilot--build-command prompt resume-id log-dir))
         (proc (apply #'start-process "copilot-cli" buf cmd)))
    (message (format "Calling copilot like =%s=" cmd))
    (process-put proc 'copilot-log-dir log-dir)
    (set-process-filter proc #'copilot--process-filter)
    (set-process-sentinel proc (lambda (p e)
                                 (copilot--process-sentinel p e)
                                 (unless (copilot--current-session-id buf)
                                   (let* ((ld (process-get p 'copilot-log-dir))
                                          (sid (and ld (copilot--discover-session-id ld))))
                                     (when sid
                                       (process-put p 'copilot-session-id sid)
                                       (copilot--set-session-id buf sid))))))
    proc))

(defun copilot--prepare-buffer (prompt)
  (let* ((buf (generate-new-buffer (copilot--buffer-name prompt)))
         (ts (format-time-string "%Y-%m-%d %H:%M")))
    (with-current-buffer buf
      (org-mode)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (insert (format "#+title: %s\n\n\n" ts))))
    buf))

(defun copilot--resume-buffer (buf prompt)
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
	   (sid (copilot--current-session-id buf)))
      (goto-char (point-max))
      (copilot--insert-new-user-prompt buf prompt)
      (copilot--ensure-ai-block buf))))

(defun copilot--resume-buffer-p (buf)
  (and (derived-mode-p 'org-mode)
       (copilot--current-session-id buf)))

;;;###autoload
(defun copilot/select ()
  "Select an existing Copilot buffer using standard buffer completion.
Buffers are recognized by the naming pattern produced by `copilot--buffer-name'.
If no Copilot buffers exist, signal a user error."
  (interactive)
  (let* ((candidates (seq-filter (lambda (b)
                                   (string-match-p "^\\*Copilot \\[[^]]+\\] - " (buffer-name b)))
                                 (buffer-list)))
         (names (mapcar #'buffer-name candidates)))
    (unless names
      (user-error "No Copilot buffers"))
    (let* ((choice (completing-read "Copilot buffer: " names nil t)))
      (switch-to-buffer choice))))

;;;###autoload
(defun copilot/new (prompt)
  "Start or continue a Copilot chat session with PROMPT.
When called from an existing Copilot buffer (has session id), the
session is resumed via --resume.  Otherwise a new chat is started."
  (interactive (list (read-string "Copilot prompt: ")))
  (let* ((sid (copilot--current-session-id (current-buffer)))
	 (buf (if sid
		  (current-buffer)
		(copilot--prepare-buffer prompt))))
    (copilot--resume-buffer buf prompt)
    (copilot--start-process prompt buf sid)
    (copilot--display-buffer buf))
  (message "Copilot: started"))

(provide 'copilot)
;;; copilot.el ends here

