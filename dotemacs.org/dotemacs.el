
(setq message-log-max 16384)

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(defun add-to-load-path (path &optional dir)
  (let
      ((p (expand-file-name path (or dir user-emacs-directory))))
    (when (file-directory-p p)
      (add-to-list 'load-path p
            ))))

(defun add-to-load-path-include-subdirs (path)
  "add level 1 subdirectories of path to te load-path."
  (add-to-load-path path)
  (dolist (entry (nreverse (directory-files-and-attributes
                            (expand-file-name path user-emacs-directory))))
    (if (and (cadr entry)
             (not (equal "." (car entry)))
             (not (equal ".." (car entry)))
             )
        (add-to-load-path (car entry)
                          (expand-file-name path user-emacs-directory)))))

(setq package-enable-at-startup nil)

(defvar group-load-directories nil
   "directories that should have their subdirectories added
to the load-path. Can be absolute or relative to user-emacs-directory")

(setq group-load-directories (list "site-lisp" "elpa"))

(mapc #'add-to-load-path-include-subdirs
      (reverse group-load-directories))

(add-to-load-path (expand-file-name user-emacs-directory))

(setq custom-theme-directory
  (expand-file-name "themes" user-emacs-directory))
(setq custom-theme-load-path
      (quote
       (custom-theme-directory
        t
        "/Users/tonyday/.emacs.d/elpa/noctilux-theme-20131019.31/"
        "/Users/tonyday/.emacs.d/elpa/zenburn-theme-2.0"
        "/Users/tonyday/.emacs.d/themes/")))
(load-theme 'noctilux t)
;;(load-theme 'zenburn t)
;;(load-theme 'zenburn-overrides t)
;;(load-theme 'system-type-darwin t)

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))
(use-package bind-key)

(server-start)

;; supress toolbar
(use-package td-startup-look-and-feel)

(use-package td-utils)

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;; Main keymaps for personal bindings are:
;;
;;   C-x <letter>  primary map (has many defaults too)
;;   C-c <letter>  secondary map (not just for mode-specific)
;;   C-. <letter>  tertiary map
;;
;;   M-g <letter>  goto map
;;   M-s <letter>  search map
;;   M-o <letter>  markup map (even if only temporarily)
;;
;;   C-<capital letter>
;;   M-<capital letter>
;;
;;   A-<anything>
;;   M-A-<anything>
;;
;; Single-letter bindings still available:
;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;   M- ?#

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

(bind-key* "<C-return>" 'other-window)
(bind-key "C-w" 'backward-kill-word)

(bind-key "M-/" 'dabbrev-expand)

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(bind-key "M-[" 'align-code)
(bind-key "M-`" 'other-frame)

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-j" 'delete-indentation-forward)
(bind-key "M-J" 'delete-indentation)

(bind-key "M-e" 'copy-word)

(defun copy-word (&optional arg)
  (interactive "p")
  (mark-word)
  (kill-ring-save (point) (mark)))

(bind-key "M-W" 'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)

(defun delete-leading-whitespace ()
  "delete leading whitespace from current buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "[^
]")
    (delete-region (point-min)  (- (point) 1))))

(bind-key "C-M-S-s-L" 'delete-leading-whitespace)
(bind-key "C-M-S-s-T" 'delete-trailing-whitespace)
(bind-key "C-M-S-s-w" 'whitespace-cleanup)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-H" 'mark-paragraph)
(bind-key "M-D" 'mark-defun)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)

(bind-key "M-s n" 'find-name-dired)
(bind-key "M-s o" 'occur)

(bind-key "M-x" 'helm-M-x)
(bind-key "C-c M-x" 'smex)
(bind-key "C-x x" 'smex)

(bind-key "<C-M-backspace>" 'backward-kill-sexp)

(defun isearch-backward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-backward))

(bind-key "C-M-r" 'isearch-backward-other-window)

(defun isearch-forward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-forward))

(bind-key "C-M-s" 'isearch-forward-other-window)

;; Some further isearch bindings
(bind-key "C-c" 'isearch-toggle-case-fold isearch-mode-map)
(bind-key "C-t" 'isearch-toggle-regexp isearch-mode-map)
(bind-key "C-^" 'isearch-edit-string isearch-mode-map)
(bind-key "C-i" 'isearch-complete isearch-mode-map)

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

(bind-key "<s-return>" 'swap-windows)

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond
   ((not (= (count-windows) 2))
    (message "You need exactly 2 windows to do this."))
   (t
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))

(bind-key "C-x B" 'ido-switch-buffer-other-window)
(bind-key "C-x d" 'delete-whitespace-rectangle)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(bind-key "C-x 4 t" 'transpose-windows)

(defun set-transparency-clear ()
  (set-frame-parameter nil 'alpha 100))

(defun set-transparency (&optional alpha-level)
  (interactive "p")
  (let ((alpha-level
         (if (or (not alpha-level)
                 (< alpha-level 2))
             (read-number "Opacity percentage: " 85)
           alpha-level
           )))
    (set-frame-parameter nil 'alpha alpha-level))
  (message (format "Alpha level is %d" (frame-parameter nil 'alpha))))

(defun emacs-toggle-transparency ()
  (interactive)
  (if (< (frame-parameter nil 'alpha) 100)
      (set-transparency-clear)
    (set-transparency)))

(bind-key "C-x 5 t" 'emacs-toggle-transparency)

(dolist
    (r `((?d (file . "~/.emacs.d/dotemacs.org"))
         (?s (file . "~/.emacs.d/settings.el"))
         (?b (file . "~/stuff/org/bugz.org"))
         (?h (file . "~/.bash_history"))

         ))
  (set-register (car r) (cadr r)))

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" 'duplicate-line)
(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-k" 'kill-region)

(bind-key "C-x C-n" 'next-line)

(defun find-alternate-file-with-sudo (filename)
  (interactive
   (list (read-file-name "Find alternate file: " nil
                         nil nil (concat "/sudo::" (buffer-file-name)))))
  (find-alternate-file filename))

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)

(bind-key "C-x M-n" 'set-goal-column)

(defun refill-paragraph (arg)
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil)
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(bind-key "C-x M-q" 'refill-paragraph)

(bind-key "C-c <tab>" 'ff-find-other-file)

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" 'delete-current-line)

(bind-key "C-c e E" 'elint-current-buffer)

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(bind-key "C-c e b" 'do-eval-buffer)
(bind-key "C-c e c" 'cancel-debug-on-entry)
(bind-key "C-c e d" 'debug-on-entry)
(bind-key "C-c e e" 'toggle-debug-on-error)
(bind-key "C-c e f" 'emacs-lisp-byte-compile-and-load)
(bind-key "C-c e j" 'emacs-lisp-mode)
(bind-key "C-c e l" 'find-library)
(bind-key "C-c e r" 'eval-region)
(bind-key "C-c e s" 'scratch)
(bind-key "C-c e v" 'edit-variable)

(defun find-which (name)
  (interactive "sCommand name: ")
  (find-file-other-window
   (substring (shell-command-to-string (format "which %s" name)) 0 -1)))

(bind-key "C-c e w" 'find-which)
(bind-key "C-c e z" 'byte-recompile-directory)

(bind-key "C-c f" 'helm-recentf)

(bind-key "C-c g" 'goto-line)

(bind-key "C-c k" 'keep-lines)



(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)

(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)

(autoload 'auth-source-search "auth-source")

(defun tinify-url (url)
  (interactive "sURL to shorten: ")
  (let* ((api-login "tonyday567")
         (api-key
          (funcall
           (plist-get
            (car (auth-source-search :host "api.j.mp" :login api-login
                                     :port 80))
            :secret))))
    (cl-flet ((message (&rest ignore)))
      (with-current-buffer
          (let ((query
                 (format "format=txt&longUrl=%s&login=%s&apiKey=%s"
                         (url-hexify-string url) api-login api-key)))
            (url-retrieve-synchronously
             (concat "http://api.j.mp/v3/shorten?" query)))
        (goto-char (point-min))
        (re-search-forward "^$")
        (prog1
            (kill-new (buffer-substring (1+ (point)) (1- (point-max))))
          (kill-buffer (current-buffer)))))))

(bind-key "C-c U" 'tinify-url)

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c z" 'clean-buffer-list)

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)

(defvar ctl-c-t-map)
(define-prefix-command 'ctl-c-t-map)
(bind-key "C-c t" 'ctl-c-t-map)

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

(defun unfill-paragraph (arg)
  (interactive "*p")
  (let (beg end)
    (forward-paragraph arg)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph arg)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (when (> (count-lines beg end) 1)
      (while (< (point) end)
        (goto-char (line-end-position))
        (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
          (delete-indentation 1)
          (if sent-end
              (insert ? )))
        (end-of-line))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
          (replace-match " " nil nil nil 1))))))

(bind-key "C-c M-q" 'unfill-paragraph)

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))



(bind-key "C-. m" 'kmacro-keymap)

(defun check-html5 ()
    (interactive)
    (save-buffer)
    (shell-command (concat "html5check.py " (shell-quote-argument (buffer-file-name)))))

(bind-key "C-. h" 'check-html5)

(defun td-set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(bind-key "C-. t" 'td-set-truncate-lines)

(bind-key "C-. C-i" 'indent-rigidly)

(bind-key "C-. C-c" 'org-indent-indent-buffer)

(defvar ctl-period-ctl-period-map)
(define-prefix-command 'ctl-period-ctl-period-map)
(bind-key "C-. C-." 'ctl-period-ctl-period-map)
(bind-key "C-. C-. l" 'flush-lines)
(bind-key "C-. C-. d" 'describe-personal-keybindings)
(bind-key "C-. C-. k" 'keyfreq-show)

(defun td-face-at-point ()
  (interactive)
  (alert (format "%s" (face-at-point))))

(bind-key "C-. C-. f" 'td-face-at-point)

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)
(bind-key "C-h e" 'lisp-find-map)

(bind-key "C-h e c" 'finder-commentary)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)

(defun td-describe-symbol  (symbol &optional mode)
  (interactive
   (info-lookup-interactive-arguments 'symbol current-prefix-arg))
  (let (info-buf find-buf desc-buf cust-buf)
    (save-window-excursion
      (ignore-errors
        (info-lookup-symbol symbol mode)
        (setq info-buf (get-buffer "*info*")))
      (let ((sym (intern-soft symbol)))
        (when sym
          (if (functionp sym)
              (progn
                (find-function sym)
                (setq find-buf (current-buffer))
                (describe-function sym)
                (setq desc-buf (get-buffer "*Help*")))
            (find-variable sym)
            (setq find-buf (current-buffer))
            (describe-variable sym)
            (setq desc-buf (get-buffer "*Help*"))
            ;;(customize-variable sym)
            ;;(setq cust-buf (current-buffer))
            ))))

    (delete-other-windows)

    (cl-flet ((switch-in-other-buffer
            (buf)
            (when buf
              (split-window-vertically)
              (switch-to-buffer-other-window buf))))
      (switch-to-buffer find-buf)
      (switch-in-other-buffer desc-buf)
      (switch-in-other-buffer info-buf)
      ;;(switch-in-other-buffer cust-buf)
      (balance-windows))))

(bind-key "C-h e d" 'td-describe-symbol)
(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)

(defvar lisp-modes  '(emacs-lisp-mode
                      inferior-emacs-lisp-mode
                      ielm-mode
                      lisp-mode
                      inferior-lisp-mode
                      lisp-interaction-mode
                      slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
        (funcall current-mode))))

(bind-key "C-h e s" 'scratch)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)

(defun td-indent-region ()
    (interactive)
    (save-excursion
      (save-restriction
        (if (> (point) (mark))
            (exchange-point-and-mark))
        (while (< (point) (mark))
          (indent-for-tab-command)
          (forward-line 1)))))

  (bind-key "C-M-S-s-i r" 'td-indent-region)

  (defun td-indent-buffer ()
    (interactive)
    (save-excursion
      (save-restriction
        (mark-whole-buffer)
        (td-indent-region))))

  (bind-key "C-M-S-s-i b" 'td-indent-buffer)

  (bind-key "C-M-S-s-n" 'new-frame)

(defun firefox-refresh ()
  (interactive)
  (do-applescript
   (concat
    "set frontmostApplication to path to frontmost application\n"
    "tell application \"Firefox\"\n"
    "   activate\n"
    "   delay 0.15\n"
    "   tell application \"System Events\"\n"
    "           keystroke \"r\" using {command down}\n"
    "   end tell\n"
    "   delay 0.15\n"
    "end tell\n"
    "activate application (frontmostApplication as text)\n")))

  (bind-key "C-M-S-s-." 'firefox-refresh)

(use-package ace-jump-mode
  :bind ("C-. C-s" . ace-jump-mode))

(use-package alert
  :init (alert "alert firing up"))

(use-package allout
  :diminish allout-mode
  :commands allout-mode
  :config
  (progn
    (defvar allout-unprefixed-keybindings nil)
    (defvar allout-command-prefix "C-M-S-s-c")

    (defun td-allout-mode-hook ()
      (dolist (mapping '((?b . allout-hide-bodies)
                         (?c . allout-hide-current-entry)
                         (?l . allout-hide-current-leaves)
                         (?i . allout-show-current-branches)
                         (?e . allout-show-entry)
                         (?o . allout-show-to-offshoot)))
        (eval `(bind-key ,(concat (format-kbd-macro allout-command-prefix)
                                  " " (char-to-string (car mapping)))
                         (quote ,(cdr mapping))
                         allout-mode-map))))

    (add-hook 'allout-mode-hook 'td-allout-mode-hook)))

(use-package tex-site
  :load-path "/Users/tonyday/.emacs.d/site-lisp/auctex/"
  :defines (latex-help-cmd-alist
            latex-help-file)
  ;; jww (2012-06-15): Do I want to use AucTeX for texinfo-mode?
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (progn
    (defun latex-help-get-cmd-alist () ;corrected version:
      "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
      ;; mm, does it contain any cached entries
      (if (not (assoc "\\begin" latex-help-cmd-alist))
          (save-window-excursion
            (setq latex-help-cmd-alist nil)
            (Info-goto-node (concat latex-help-file "Command Index"))
            (goto-char (point-max))
            (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
              (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                    (value (buffer-substring (match-beginning 2)
                                             (match-end 2))))
                (add-to-list 'latex-help-cmd-alist (cons key value))))))
      latex-help-cmd-alist)

    (use-package latex-mode
      :config
      (info-lookup-add-help :mode 'latex-mode
                            :regexp ".*"
                            :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                            :doc-spec '(("(latex2e)Concept Index" )
                                        ("(latex2e)Command Index"))))))

(use-package auto-complete-config
  :commands auto-complete-mode
  :diminish auto-complete-mode
  :config
  (progn
    (ac-set-trigger-key "TAB")
    (setq ac-use-menu-map t)
    (unbind-key "C-s" ac-completing-map)))

(use-package autorevert
  :init (global-auto-revert-mode t))

(use-package backup-each-save
  :defer t
  :init
  (progn
    (autoload 'backup-each-save "backup-each-save")
    (add-hook 'after-save-hook 'backup-each-save)

    (defun td-make-backup-file-name (file)
      (make-backup-file-name-1 (file-truename file)))

    (defun show-backups ()
      (interactive)
      (require 'find-dired)
      (let* ((file (make-backup-file-name (buffer-file-name)))
             (dir (file-name-directory file))
             (args (concat "-iname '" (file-name-nondirectory file)
                           ".~*~'"))
             (dired-buffers dired-buffers)
             (find-ls-option '("-print0 | xargs -0 ls -lta" . "-lta")))
        ;; Check that it's really a directory.
        (or (file-directory-p dir)
            (error "Backup directory does not exist: %s" dir))
        (with-current-buffer (get-buffer-create "*Backups*")
          (let ((find (get-buffer-process (current-buffer))))
            (when find
              (if (or (not (eq (process-status find) 'run))
                      (yes-or-no-p "A `find' process is running; kill it? "))
                  (condition-case nil
                      (progn
                        (interrupt-process find)
                        (sit-for 1)
                        (delete-process find))
                    (error nil))
                (error "Cannot have two processes in `%s' at once"
                       (buffer-name)))))

          (widen)
          (kill-all-local-variables)
          (setq buffer-read-only nil)
          (erase-buffer)
          (setq default-directory dir
                args (concat find-program " . "
                             (if (string= args "")
                                 ""
                               (concat
                                (shell-quote-argument "(")
                                " " args " "
                                (shell-quote-argument ")")
                                " "))
                             (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
                                               (car find-ls-option))
                                 (format "%s %s %s"
                                         (match-string 1 (car find-ls-option))
                                         (shell-quote-argument "{}")
                                         find-exec-terminator)
                               (car find-ls-option))))
          ;; Start the find process.
          (message "Looking for backup files...")
          (shell-command (concat args "&") (current-buffer))
          ;; The next statement will bomb in classic dired (no optional arg
          ;; allowed)
          (dired-mode dir (cdr find-ls-option))
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map (current-local-map))
            (define-key map "\C-c\C-k" 'kill-find)
            (use-local-map map))
          (make-local-variable 'dired-sort-inhibit)
          (setq dired-sort-inhibit t)
          (set (make-local-variable 'revert-buffer-function)
               `(lambda (ignore-auto noconfirm)
                  (find-dired ,dir ,find-args)))
          ;; Set subdir-alist so that Tree Dired will work:
          (if (fboundp 'dired-simple-subdir-alist)
              ;; will work even with nested dired format (dired-nstd.el,v 1.15
              ;; and later)
              (dired-simple-subdir-alist)
            ;; else we have an ancient tree dired (or classic dired, where
            ;; this does no harm)
            (set (make-local-variable 'dired-subdir-alist)
                 (list (cons default-directory (point-min-marker)))))
          (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches)
          (setq buffer-read-only nil)
          ;; Subdir headlerline must come first because the first marker in
          ;; subdir-alist points there.
          (insert "  " dir ":\n")
          ;; Make second line a ``find'' line in analogy to the ``total'' or
          ;; ``wildcard'' line.
          (insert "  " args "\n")
          (setq buffer-read-only t)
          (let ((proc (get-buffer-process (current-buffer))))
            (set-process-filter proc (function find-dired-filter))
            (set-process-sentinel proc (function find-dired-sentinel))
            ;; Initialize the process marker; it is used by the filter.
            (move-marker (process-mark proc) 1 (current-buffer)))
          (setq mode-line-process '(":%s")))))

    (bind-key "C-x ~" 'show-backups))

  :config
  (progn
    (defun backup-each-save-filter (filename)
      (not (string-match
            (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                    "\\|\\.newsrc\\(\\.eld\\)?\\)")
            filename)))

    (setq backup-each-save-filter-function 'backup-each-save-filter)

    (defun td-dont-backup-files-p (filename)
      (unless (string-match filename "/\\(archive/sent/\\|recentf$\\)")
        (normal-backup-enable-predicate filename)))

    (setq backup-enable-predicate 'td-dont-backup-files-p)))

(use-package bm
  :pre-init
  (progn
    (defvar ctl-period-breadcrumb-map)
    (define-prefix-command 'ctl-period-breadcrumb-map)
    (bind-key "C-. c" 'ctl-period-breadcrumb-map))

  :bind (("C-. c b" . bm-last-in-previous-buffer)
         ("C-. c f" . bm-first-in-next-buffer)
         ("C-. c g" . bm-previous)
         ("C-. c l" . bm-show-all)
         ("C-. c c" . bm-toggle)
         ("C-. c m" . bm-toggle)
         ("C-. c n" . bm-next)
         ("C-. c p" . bm-previous)))

(use-package bookmark
  :defer t
  :config
  (progn
    (use-package bookmark+)

    (defun td-bookmark-set ()
      (interactive)
      (cl-flet ((bmkp-completing-read-lax
              (prompt &optional default alist pred hist)
              (completing-read prompt alist pred nil nil hist default)))
        (call-interactively #'bookmark-set)))

    (bind-key "C-x r m" 'td-bookmark-set)))

(use-package browse-kill-ring+
  :init
  (progn
    (browse-kill-ring-default-keybindings)
    (bind-key "C-M-S-s-k" 'browse-kill-ring)))

(use-package buffer-move
  :config
  (progn
    (bind-key* "S-C-<up>" 'buf-move-up)
    (bind-key* "S-C-<down>" 'buf-move-down)
    (bind-key* "S-C-<left>" 'buf-move-left)
    (bind-key* "S-C-<right>" 'buf-move-right)))

(use-package cedet-devel-load
    :defer t
    :load-path "~/site-lisp/cedet/contrib"
    :config (progn
              (use-package semantic/bovine/el)
              (use-package semantic/canned-configs)
              (semantic-load-enable-gaudy-code-helpers)
              ;; Activate semantic
              (semantic-mode 1)

              (setq semantic-clang-binary "/usr/local/bin/clang")
              (use-package semantic/bovine/c)
              (use-package semantic/bovine/gcc)
              (use-package semantic/bovine/clang)
              (use-package semantic/ia)
              (use-package semantic/decorate/include)
              (use-package semantic/lex-spp)
              (use-package eassist)
              (use-package auto-complete)


              ;; semantic
(defun td-cedet-hook ()
(bind-key "C-c ?" 'semantic-ia-complete-symbol (current-local-map))
(bind-key "C-c >" 'semantic-complete-analyze-inline (current-local-map))
(bind-key "C-c =" 'semantic-decoration-include-visit (current-local-map))
(bind-key "C-c j" 'semantic-ia-fast-jump (current-local-map))
(bind-key "C-c q" 'semantic-ia-show-doc (current-local-map))
(bind-key "C-c s" 'semantic-ia-show-summary (current-local-map))
(bind-key "C-c p" 'semantic-analyze-proto-impl-toggle (current-local-map))
                (add-to-list 'ac-sources 'ac-source-semantic))



  ;; (add-hook 'semantic-init-hooks 'alexott/cedet-hook)
  (add-hook 'c-mode-common-hook 'td-cedet-hook)
  (add-hook 'lisp-mode-hook 'td-cedet-hook)
  (add-hook 'scheme-mode-hook 'td-cedet-hook)
  (add-hook 'emacs-lisp-mode-hook 'td-cedet-hook)

  (defun td-c-mode-cedet-hook ()
   ;; (local-set-key "." 'semantic-complete-self-insert)
   ;; (local-set-key ">" 'semantic-complete-self-insert)
(bind-key "C-c t" 'eassist-switch-h-cpp (current-local-map))
(bind-key "C-x t" 'eassist-switch-h-cpp (current-local-map))
(bind-key "C-c e" 'eassist-list-methods (current-local-map))
(bind-key "C-c C-r" 'semantic-symref (current-local-map))

  ;;  (add-to-list 'ac-sources 'ac-source-etags)
    (add-to-list 'ac-sources 'ac-source-gtags)
    )
  (add-hook 'c-mode-common-hook 'td-c-mode-cedet-hook)

  (use-package cedet-global)
  (when (cedet-gnu-global-version-check t)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))

  (semanticdb-enable-cscope-databases :noerror)

  (ignore-errors
    (when (cedet-ectag-version-check t)
      (semantic-load-enable-primary-ectags-support)))

  ;; SRecode
  (global-srecode-minor-mode 1)

  ;; EDE
  (global-ede-mode 1)
  (ede-enable-generic-projects)

  (defun recur-list-files (dir re)
    "Returns list of files in directory matching to given regex"
    (when (file-accessible-directory-p dir)
      (let ((files (directory-files dir t))
            matched)
        (dolist (file files matched)
          (let ((fname (file-name-nondirectory file)))
            (cond
             ((or (string= fname ".")
                  (string= fname "..")) nil)
             ((and (file-regular-p file)
                   (string-match re fname))
              (setq matched (cons file matched)))
             ((file-directory-p file)
              (let ((tfiles (recur-list-files file re)))
                (when tfiles (setq matched (append matched tfiles)))))))))))

  (defun c++-setup-boost (boost-root)
    (when (file-accessible-directory-p boost-root)
      (let ((cfiles (recur-list-files boost-root "\\(config\\|user\\)\\.hpp")))
        (dolist (file cfiles)
          (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))



  ;; my functions for EDE
  (defun alexott/ede-get-local-var (fname var)
    "fetch given variable var from :local-variables of project of file fname"
    (let* ((current-dir (file-name-directory fname))
           (prj (ede-current-project current-dir)))
      (when prj
        (let* ((ov (oref prj local-variables))
              (lst (assoc var ov)))
          (when lst
            (cdr lst))))))

  ;; setup compile package
  (use-package compile)
  (setq compilation-disable-input nil)
  (setq compilation-scroll-output t)
  (setq mode-compile-always-save-buffer-p t)

  (defun alexott/compile ()
    "Saves all unsaved buffers, and runs 'compile'."
    (interactive)
    (save-some-buffers t)
    (let* ((r (alexott/ede-get-local-var
               (or (buffer-file-name (current-buffer)) default-directory)
               'compile-command))
           (cmd (if (functionp r) (funcall r) r)))
      (set (make-local-variable 'compile-command) (or cmd compile-command))
      (compile compile-command)))

  (defun alexott/gen-std-compile-string ()
    "Generates compile string for compiling CMake project in debug mode"
    (let* ((current-dir (file-name-directory
                         (or (buffer-file-name (current-buffer)) default-directory)))
           (prj (ede-current-project current-dir))
           (root-dir (ede-project-root-directory prj)))
      (concat "cd " root-dir "; make -j2")))

  (defun alexott/gen-cmake-debug-compile-string ()
    "Generates compile string for compiling CMake project in debug mode"
    (let* ((current-dir (file-name-directory
                         (or (buffer-file-name (current-buffer)) default-directory)))
           (prj (ede-current-project current-dir))
           (root-dir (ede-project-root-directory prj))
           (subdir "")
           )
      (when (string-match root-dir current-dir)
        (setf subdir (substring current-dir (match-end 0))))
      (concat "cd " root-dir "Debug/" "; make -j3")))

  (defun alexott/gen-cmake-debug/release-compile-string ()
    "Generates compile string for compiling CMake project in debug & release modes"
    (let* ((current-dir (file-name-directory
                         (or (buffer-file-name (current-buffer)) default-directory)))
           (prj (ede-current-project current-dir))
           (root-dir (ede-project-root-directory prj))
           (subdir "")
           )
      (when (string-match root-dir current-dir)
        (setf subdir (substring current-dir (match-end 0))))
      (concat "cd " root-dir "Debug/ && make -j3 && cd " root-dir "Release/ && make -j3" )))

  ;; Projects
  (when (file-exists-p "~/projects/opoker/CMakeLists.txt")
  (setq opoker-project
  (ede-cpp-root-project "opoker"
                  :name "opoker rescue"
                  :file "~/projects/opoker/CMakeLists.txt"
                  :include-path '("/include"
                                  "../../gtest-1.5.0/include/gtest"
                                 )
                  :system-include-path '("/usr/include/c++/4.2.1"))))
(when (file-exists-p "~/projects/opoker/CMakeLists.txt")
  (setq iqtest-project
  (ede-cpp-root-project "iqtest"
                  :name "iqfeed initial test"
                  :file "~/projects/iqfeed/CMakeLists.txt"
                  :include-path '("/include"
                                 )
                  :system-include-path '("/usr/include/c++/4.2.1"))))
))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(use-package cmake-project
:init (progn
  (defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)))

(use-package coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode))
  :init
  (progn
    (use-package flycheck)
    (add-hook 'coffee-mode-hook 'td-coffee-hook)
    (defun td-coffee-hook ()
      (flycheck-mode)
      (yas-minor-mode 1)
      (skewer-mode)
      (set (make-local-variable 'yas-fallback-behavior) 'call-other-command))))

(use-package compile
  :defer t
  :config
  (add-hook 'compilation-finish-functions
            (lambda (buf why)
              (display-buffer buf))))

(use-package cpputils-cmake)

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.less\\'" . css-mode))
  :init (progn
          (use-package skewer-css)
          (add-hook 'css-mode-hook 'skewer-css-mode)))

(use-package ibuffer
  :defer t
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))
    :bind ("C-x C-b" . ibuffer))

(use-package dedicated
  :bind ("C-. d" . dedicated-mode))

(use-package diff-mode
  :commands diff-mode
  :config
  (use-package diff-mode-))

(use-package dired
 :defer t
 :config
(progn
  (setq insert-directory-program "gls")
  ;;(use-package ls-lisp)
  (defun dired-package-initialize ()
    (unless (featurep 'runner)
     (use-package dired-x)
     ;; (use-package dired-async)
     (use-package dired-sort-map)
     (use-package runner))

     ;;(setq dired-use-ls-dired t)
     ;;(setq ls-lisp-use-insert-directory-program nil)
     ;;(setq insert-directory-program "gls")

     (bind-key "l" 'dired-up-directory dired-mode-map)

     (defun td-dired-switch-window ()
        (interactive)
        (if (eq major-mode 'sr-mode)
            (call-interactively #'sr-change-window)
          (call-interactively #'other-window)))

      (bind-key "<tab>" 'td-dired-switch-window dired-mode-map)

      (bind-key "M-!" 'async-shell-command dired-mode-map)
      (unbind-key "M-G" dired-mode-map)
      (unbind-key "M-s f" dired-mode-map)

      (defadvice dired-omit-startup (after diminish-dired-omit activate)
        "Make sure to remove \"Omit\" from the modeline."
        (diminish 'dired-omit-mode) dired-mode-map)

      (defadvice dired-next-line (around dired-next-line+ activate)
        "Replace current buffer if file is a directory."
        ad-do-it
        (while (and  (not  (eobp)) (not ad-return-value))
          (forward-line)
          (setq ad-return-value(dired-move-to-filename)))
        (when (eobp)
          (forward-line -1)
          (setq ad-return-value(dired-move-to-filename))))

      (defadvice dired-previous-line (around dired-previous-line+ activate)
        "Replace current buffer if file is a directory."
        ad-do-it
        (while (and  (not  (bobp)) (not ad-return-value))
          (forward-line -1)
          (setq ad-return-value(dired-move-to-filename)))
        (when (bobp)
          (call-interactively 'dired-next-line)))

      (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

      ;; Omit files that Git would ignore
      (defun dired-omit-regexp ()
        (let ((file (expand-file-name ".git"))
              parent-dir)
          (while (and (not (file-exists-p file))
                      (progn
                        (setq parent-dir
                              (file-name-directory
                               (directory-file-name
                                (file-name-directory file))))
                        ;; Give up if we are already at the root dir.
                        (not (string= (file-name-directory file)
                                      parent-dir))))
            ;; Move up to the parent dir and try again.
            (setq file (expand-file-name ".git" parent-dir)))
          ;; If we found a change log in a parent, use that.
          (if (file-exists-p file)
              (let ((regexp (funcall dired-omit-regexp-orig))
                    (omitted-files
                     (shell-command-to-string "git clean -d -x -n")))
                (if (= 0 (length omitted-files))
                    regexp
                  (concat
                   regexp
                   (if (> (length regexp) 0)
                       "\\|" "")
                   "\\("
                   (mapconcat
                    #'(lambda (str)
                        (concat
                         "^"
                         (regexp-quote
                          (substring str 13
                                     (if (= ?/ (aref str (1- (length str))))
                                         (1- (length str))
                                       nil)))
                         "$"))
                    (split-string omitted-files "\n" t)
                    "\\|")
                   "\\)")))
            (funcall dired-omit-regexp-orig)))))

  (add-hook 'dired-mode-hook 'dired-package-initialize)

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (ido-read-directory-name "First directory: "
                                    (expand-file-name "~")
                                    nil nil "dl/")
           (ido-read-directory-name "Second directory: "
                                    (expand-file-name "~")
                                    nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  (bind-key "C-c J" 'dired-double-jump)))

(add-to-list 'auto-mode-alist '("\\.docx\\'" . docx2txt))

(defun docx2txt ()
  "Run docx2txt on the entire buffer."
  (shell-command-on-region (point-min) (point-max) "docx2txt.pl" t t))

(use-package ediff
  :pre-init
  (progn
    (defvar ctl-period-equals-map)
    (define-prefix-command 'ctl-period-equals-map)
    (bind-key "C-. =" 'ctl-period-equals-map)

    (bind-key "C-. = c" 'compare-windows)) ; not an ediff command, but it fits
  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  :config
  (progn
    (use-package ediff-keep)
    ;; diff hooks for org mode
    (add-hook 'ediff-select-hook 'f-ediff-org-unfold-tree-element)
    (add-hook 'ediff-unselect-hook 'f-ediff-org-fold-tree)
    ;; Check for org mode and existence of buffer
    (defun f-ediff-org-showhide(buf command &rest cmdargs)
      "If buffer exists and is orgmode then execute command"
      (if buf
          (if (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
              (save-excursion (set-buffer buf) (apply command cmdargs)))))

    (defun f-ediff-org-unfold-tree-element ()
      "Unfold tree at diff location"
      (f-ediff-org-showhide ediff-buffer-A 'org-reveal)
      (f-ediff-org-showhide ediff-buffer-B 'org-reveal)
      (f-ediff-org-showhide ediff-buffer-C 'org-reveal))
    ;;
    (defun f-ediff-org-fold-tree ()
      "Fold tree back to top level"
      (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
      (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
      (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))))

(use-package erc
  ;; :commands erc
  :commands (irc im)
  :disabled nil
  :init
  (progn
    (defun irc ()
      (interactive)
      (erc-tls :server "irc.freenode.net"
               :port 6697
               :nick "tonyday567"
               :password (funcall
                          (plist-get
                           (car (auth-source-search :host "irc.freenode.net"
                                                    :user "tonyday567"
                                                    :port 6667))
                           :secret)))))

  :config
  (progn
    (erc-track-minor-mode 1)
    (erc-track-mode 1)

    (use-package erc-alert)
    (use-package erc-highlight-nicknames)
    (use-package erc-patch)

    (use-package erc-yank
      :init
      (bind-key "C-y" 'erc-yank erc-mode-map))

    (use-package wtf
      :commands wtf-is
      :init
      (defun erc-cmd-WTF (term &rest ignore)
        "Look up definition for TERM."
        (let ((def (wtf-is term)))
          (if def
              (let ((msg (concat "{Term} " (upcase term) " is " def)))
                (with-temp-buffer
                  (insert msg)
                  (kill-ring-save (point-min) (point-max)))
                (message msg))
            (message (concat "No definition found for " (upcase term)))))))

    (use-package bitlbee
      :init (bitlbee-start))

    (defun switch-to-bitlbee ()
      (interactive)
      (switch-to-buffer-other-window "&bitlbee")
      (call-interactively 'erc-channel-names)
      (goto-char (point-max)))

    (bind-key "C-. C-. b" 'switch-to-bitlbee)

    (defun erc-cmd-SHOW (&rest form)
      "Eval FORM and send the result and the original form as:
FORM => (eval FORM)."
      (let* ((form-string (mapconcat 'identity form " "))
             (result
              (condition-case err
                  (eval (read-from-whole-string form-string))
                (error
                 (format "Error: %s" err)))))
        (erc-send-message (format "%s => %S" form-string result))))

    (defun erc-cmd-INFO (&rest ignore)
      "Send current info node."
      (unless (get-buffer "*info*")
        (error "No *info* buffer"))
      (let (output)
        (with-current-buffer "*info*"
          (let* ((file (file-name-nondirectory Info-current-file))
                 (node Info-current-node))
            (setq output (format "(info \"(%s)%s\") <-- hit C-x C-e to evaluate"
                                 file node))))
        (erc-send-message output)))

    (eval-when-compile
      (defvar erc-fools))

    (defun erc-cmd-FOOL (term &rest ignore)
      (add-to-list 'erc-fools term))

    (defun erc-cmd-UNFOOL (term &rest ignore)
      (setq erc-fools (delete term erc-fools)))

    (defun erc-cmd-OPME ()
      "Request chanserv to op me."
      (erc-message "PRIVMSG"
                   (format "chanserv op %s %s"
                           (erc-default-target)
                           (erc-current-nick)) nil))

    (defun erc-cmd-DEOPME ()
      "Deop myself from current channel."
      (erc-cmd-DEOP (format "%s" (erc-current-nick))))))

(use-package eshell
  :defer t
  :init
  (progn
    (defun eshell-initialize ()
      (defun eshell-spawn-external-command (beg end)
        "Parse and expand any history references in current input."
        (save-excursion
          (goto-char end)
          (when (looking-back "&!" beg)
            (delete-region (match-beginning 0) (match-end 0))
            (goto-char beg)
            (insert "spawn "))))

      (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

      (defun ss (server)
        (interactive "sServer: ")
        (call-process "spawn" nil nil nil "ss" server))

      (eval-after-load "em-unix"
        '(progn
           (unintern 'eshell/su)
           (unintern 'eshell/sudo))))

    (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)))

(use-package esh-toggle
  :requires eshell
  :bind ("C-x C-z" . eshell-toggle))

(use-package ess-site
  :mode ("\\.[rR]\\'" . R-mode)
  :bind ("C-. C-. r" . R))
;;(use-package ess-tracebug)

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (progn
    (setq eval-expr-print-function 'pp
          eval-expr-print-level 20
          eval-expr-print-length 100)

    (defun eval-expr-minibuffer-setup ()
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (paredit-mode))))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package ffap
:init (progn
(setq ffap-machine-p-known 'accept) ; no pinging
(setq ffap-url-regexp nil) ; disable URL features in ffap
(setq ffap-ftp-regexp nil) ; disable FTP features in ffap
)
  :bind ("C-c v" . ffap))

(use-package find-file-in-project
  :bind ("C-x f" . find-file-in-project))

(use-package ispell
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config
  (define-key flyspell-mode-map [(control ?.)] nil))

(use-package gist
  :bind ("C-c G" . gist-region-or-buffer))

(use-package god-mode
  :bind (("C-x C-G" . god-local-mode)
         ("C-x C-g" . god-mode)))

(use-package goto-last-change
  :bind ("C-x C-x" . goto-last-change))

(use-package graphviz-dot-mode
 :mode ("\\.dot\\'" . graphviz-dot-mode))

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s f" . find-grep)
         ("M-s g" . grep)
         ("M-s r" . rgrep))
  :init
  (progn
    (defun find-grep-in-project (command-args)
      (interactive
       (let ((default (thing-at-point 'symbol)))
         (list (read-shell-command "Run find (like this): "
                                   (cons (concat "git --no-pager grep -n "
                                                 default)
                                         (+ 24 (length default)))
                                   'grep-find-history))))
      (if command-args
          (let ((null-device nil))      ; see grep
            (grep command-args))))

    (bind-key "M-s p" 'find-grep-in-project))

  :config
  (progn
    (use-package grep-ed)

    (grep-apply-setting 'grep-command "egrep -nH -e ")
    (grep-apply-setting
     'grep-find-command
     '("find . -type f -print0 | xargs -P4 -0 egrep -nH -e " . 52))))

(use-package gud
  :commands gud-gdb
  :init
  (progn
    (defun show-debugger ()
      (interactive)
      (let ((gud-buf
             (catch 'found
               (dolist (buf (buffer-list))
                 (if (string-match "\\*gud-" (buffer-name buf))
                     (throw 'found buf))))))
        (if gud-buf
            (switch-to-buffer-other-window gud-buf)
          (call-interactively 'gud-gdb))))

    (bind-key "C-. g" 'show-debugger))

  :config
  (progn
    (bind-key "<f9>" 'gud-cont)
    (bind-key "<f10>" 'gud-next)
    (bind-key "<f11>" 'gud-step)
    (bind-key "S-<f11>" 'gud-finish)))

(use-package haskell-cabal
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
    (defvar haskell-cabal-mode-map)
    (define-prefix-command 'haskell-cabal-mode-map)
    (bind-key "C-c C-c" 'haskell-process-cabal-build haskell-cabal-mode-map)
    (bind-key "C-c c" 'haskell-process-cabal haskell-cabal-mode-map)
    (bind-key "C-`" 'haskell-interactive-bring haskell-cabal-mode-map)
    (bind-key "C-c C-z" 'haskell-interactive-switch haskell-cabal-mode-map)))

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
  :init
  (progn
    (use-package haskell-font-lock)
    (use-package shm
      :load-path "~/.emacs.d/site-lisp/structured-haskell-mode/elisp/")
    ;;(use-package haskell-indentation
    ;;  :diminish haskell-indentation-mode)
    (use-package flycheck)
    (use-package haskell-flycheck)
    (use-package haskell-doc)
    (use-package haskell-decl-scan)
    (use-package inf-haskell)
    (use-package haskell-navigate-imports)
    (use-package haskell-align-imports)
    (use-package haskell-sort-imports)
    (use-package haskell-move-nested)
    (use-package haskell-session)
    (use-package haskell-interactive-mode)
    (use-package haskell-fay)
    (use-package haskell-process)
    ;;(use-package haskell-indentation
    ;;  :diminish haskell-indentation-mode)
    ;;(use-package hi2
    ;;    :diminish hi2)

    (add-hook 'haskell-mode-hook 'td-haskell-hook)

    (defun haskell-cabal-cd-dir (&optional dir)
      "Change to project base directory"
      (let ((cabal-dir (haskell-cabal-find-dir dir)))
        (when cabal-dir
          (cd cabal-dir))))

    (defun td-haskell-hook ()
      ;;(turn-on-hi2)
      ;; (turn-on-haskell-indentation)
      (structured-haskell-mode)
      (turn-on-haskell-font-lock)
      (flycheck-mode)
      (yas-minor-mode 1)
      (set (make-local-variable 'yas-fallback-behavior) 'call-other-command)
      (add-hook 'flycheck-before-syntax-check-hook 'haskell-cabal-cd-dir))

    (use-package haskell-cabal
      :init
      (progn
        (add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
        (defvar haskell-cabal-mode-map)
        (define-prefix-command 'haskell-cabal-mode-map)
        (bind-key "C-c C-c" 'haskell-process-cabal-build haskell-cabal-mode-map)
        (bind-key "C-c c" 'haskell-process-cabal haskell-cabal-mode-map)))

    (define-prefix-command 'haskell-mode-map)
    (bind-key "C-c C-f" 'flycheck-buffer haskell-mode-map)
    (bind-key "C-c C-l" 'inferior-haskell-load-file haskell-mode-map)
    (define-prefix-command 'haskell-import-map)
    (bind-key "C-c y" 'haskell-import-map haskell-mode-map)
    (bind-key "C-c y a" 'haskell-align-imports haskell-mode-map)
    (bind-key "C-c y s" 'haskell-sort-imports haskell-mode-map)
    (bind-key "C-c C-s" 'haskell-decl-scan-mode haskell-mode-map)
    (bind-key "C-c C-m" 'helm-imenu haskell-mode-map)
    (bind-key "C-c C-z" 'switch-to-haskell haskell-mode-map)
    (bind-key "C-c C-r" 'td-haskell-load-and-run haskell-mode-map)
    (bind-key "C-c C-j" 'td-inferior-haskell-find-definition haskell-mode-map)
    (bind-key "C-c C-h" 'td-inferior-haskell-find-haddock haskell-mode-map)
    (bind-key "C-c C-b" 'td-inferior-haskell-break haskell-mode-map)
    (bind-key "C-c C-d" 'haskell-doc-mode haskell-mode-map)
    (bind-key "C-c C-t" 'td-inferior-haskell-type haskell-mode-map)
    (bind-key "C-c C-i" 'inferior-haskell-info haskell-mode-map)
    (bind-key "C-c C-p" 'flycheck-mode haskell-mode-map)
    (bind-key "C-c [" 'align-code haskell-mode-map)
    (bind-key "C-c g" 'haskell-process-generate-tags haskell-mode-map)
    (bind-key "M-p" 'flycheck-previous-error haskell-mode-map)
    (bind-key "M-n" 'flycheck-next-error haskell-mode-map)
    (bind-key "C-c t" 'haskell-flycheck-insert-type-binding haskell-mode-map)
    (bind-key "C-c l" 'haskell-flycheck-insert-lints haskell-mode-map)

    (defun td-haskell-load-and-run ()
      "Loads and runs the current Haskell file."
      (interactive)
      (inferior-haskell-load-and-run inferior-haskell-run-command)
      (sleep-for 0 100)
      (goto-char (point-max)))

    (defun td-inferior-haskell-find-definition ()
      "Jump to the definition immediately, the way that SLIME does."
      (interactive)
      (inferior-haskell-find-definition (haskell-ident-at-point))
      (forward-char -1))

    (defun td-inferior-haskell-find-haddock (sym)
      (interactive
       (let ((sym (haskell-ident-at-point)))
         (list (read-string
                (if (> (length sym) 0)
                    (format "Find documentation of (default %s): " sym)
                  "Find documentation of: ")
                nil nil sym))))
      (inferior-haskell-find-haddock sym)
      (goto-char (point-min))
      (search-forward (concat sym " ::") nil t)
      (search-forward (concat sym " ::") nil t)
      (goto-char (match-beginning 0)))

    (defun td-inferior-haskell-type (expr &optional insert-value)
      "Query the haskell process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `haskell-doc-mode'."
      (interactive
       (let ((sym (haskell-ident-at-point)))
         (list (if current-prefix-arg
                   (read-string (if (> (length sym) 0)
                                    (format "Show type of (default %s): " sym)
                                  "Show type of: ")
                                nil nil sym)
                 sym )
               current-prefix-arg)))
      (if (string-match "\\`\\s_+\\'" expr) (setq expr (concat "(" expr ")")))
      (let ((type (inferior-haskell-get-result (concat ":type " expr))))
        (if (not (string-match (concat "^\\(" (regexp-quote expr)
                                       "[ \t\n]+::[ \t\n]*\\(.\\|\n\\)*\\)")
                               type))
            (error "No type info: %s" type)
          (progn
            (setf type (match-string 1 type))
            ;; Cache for reuse by haskell-doc.
            (when
                (and (boundp 'haskell-doc-mode) haskell-doc-mode
                     (boundp 'haskell-doc-user-defined-ids)
                     ;; Haskell-doc only works for idents, not arbitrary expr.
                     (string-match
                      "\\`(?\\(\\s_+\\|\\(\\sw\\|\\s'\\|\\.\\)+\\)?[ \t]*::[ \t]*"
                      type))
              (let ((sym (match-string 1 type)))
                (setq haskell-doc-user-defined-ids
                      (cons (cons sym (substring type (match-end 0)))
                            (delq (assoc sym haskell-doc-user-defined-ids)
                                  haskell-doc-user-defined-ids)))))
            (if (called-interactively-p 'any) (message "%s" type))
            (when insert-value
              (beginning-of-line)
              (insert type "\n"))
            type))))

    (defun td-inferior-haskell-break (&optional arg)
      (interactive "P")
      (let ((line (line-number-at-pos))
            (col (if arg
                     ""
                   (format " %d" (current-column))))
            (proc (inferior-haskell-process)))
        (inferior-haskell-send-command
         proc (format ":break %d%s" line col))
        (message "Breakpoint set at %s:%d%s"
                 (file-name-nondirectory (buffer-file-name)) line col)))

    (defcustom haskell-mode-message-line-multi t
      "allow multiple lines in mini-buffer")
    (defadvice haskell-mode-message-line (around multi-line
                                                 activate)
      (if haskell-mode-message-line-multi
          (message "%s" (ad-get-arg 0))
        (ad-do-it (ad-get-arg 0))))

    (defconst haskell-unicode-conversions
      '(("[ (]\\(->\\)[) \n]"     . ?→)
        ("[ (]\\(/=\\)[) ]"       . ?≠)
        ;;("[ (]\\(<=\\)[) ]"       . ?≤)
        ;;("[ (]\\(>=\\)[) ]"       . ?≥)
        ;;("[ (]\\(=\\)[) ]"        . ?≡)
        ("[ (]\\(\\.\\)[) ]"      . ?∘)
        ("[ (]\\(&&\\)[) ]"       . ?∧)
        ("[ (]\\(||\\)[) ]"       . ?∨)
        ("[ (]\\(\\*\\)[) ]"      . ?×)
        ("[ (]\\(\\\\\\)[(_a-z]"  . ?λ)
        (" \\(<-\\)[ \n]"         . ?←)
        (" \\(-<\\) "             . ?↢)
        (" \\(>-\\) "             . ?↣)
        (" \\(=>\\)[ \n]"         . ?⇒)
        ;;(" \\(>=>\\) "           . ?↣)
        ;;(" \\(<=<\\) "           . ?↢)
        ;;(" \\(>>=\\) "           . ?↦)
        ;;(" \\(=<<\\) "           . ?↤)
        ("[ (]\\(\\<not\\>\\)[ )]" . ?¬)
        ;;("[ (]\\(<<<\\)[ )]"      . ?⋘)
        ;;("[ (]\\(>>>\\)[ )]"      . ?⋙)
        (" \\(::\\) "             . ?∷)
        ("\\(`union`\\)"          . ?⋃)
        ("\\(`intersect`\\)"      . ?⋂)
        ("\\(`elem`\\)"           . ?∈)
        ("\\(`notElem`\\)"        . ?∉)
        ;;("\\<\\(mempty\\)\\>"    . ??)
        ;; ("\\(`mappend`\\)"        . ?⨂)
        ;; ("\\(`msum`\\)"           . ?⨁)
        ;; ("\\(\\<True\\>\\)"       . "𝗧𝗿𝘂𝗲")
        ;; ("\\(\\<False\\>\\)"      . "𝗙𝗮𝗹𝘀𝗲")
        ("\\(\\<undefined\\>\\)"  . ?⊥)
        ("\\<\\(forall \\)\\>"   . ?∀)))

    (defvar hoogle-server-process nil)

    (defvar put-index 0)
    (defvar put-prefix "step")

    (defcustom haskell-config-use-unicode-symbols t
      "If non-nil, use Unicode symbols to represent mathematical operators."
      :type 'boolean
      :group 'haskell)

    (defface haskell-subscript '((t :height 0.6))
      "Face used for subscripts."
      :group 'haskell)

    (defun haskell-setup-unicode-conversions ()
      (if (and nil (featurep 'proof-site))
          (use-package haskell-unicode-tokens
            :load-path "site-lisp/proofgeneral/generic/"
            :config
            (hook-into-modes #'(lambda ()
                                 (ignore-errors
                                   (unicode-tokens-mode 1))
                                 (unicode-tokens-use-shortcuts 0))
                             '(haskell-mode)))
        (mapc (lambda (mode)
                (font-lock-add-keywords
                 mode
                 (append (mapcar (lambda (chars)
                                   `(,(car chars)
                                     ,(if (characterp (cdr chars))
                                          `(0 (ignore
                                               (compose-region (match-beginning 1)
                                                               (match-end 1)
                                                               ,(cdr chars))))
                                        `(0 ,(cdr chars)))))
                                 haskell-unicode-conversions)
                         '(("(\\|)" . 'esk-paren-face)
                           ;; ("\\<[a-zA-Z]+\\([0-9]\\)\\>"
                           ;;  1 haskell-subscript)
                           ))))
              '(haskell-mode))))

    (if haskell-config-use-unicode-symbols
        (haskell-setup-unicode-conversions))

    (use-package align)
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))

    (bind-key "C-<left>" (lambda ()
                           (interactive)
                           (haskell-move-nested -4))
              haskell-mode-map)
    (bind-key "A-<left>" (lambda ()
                           (interactive)
                           (haskell-move-nested -1))
              haskell-mode-map)

    (bind-key "C-<right>" (lambda ()
                            (interactive)
                            (haskell-move-nested 4))
              haskell-mode-map)
    (bind-key "A-<right>" (lambda ()
                            (interactive)
                            (haskell-move-nested 1))
              haskell-mode-map)

    (bind-key "C-c C-u" (lambda ()
                          (interactive)
                          (insert "undefined"))
              haskell-mode-map)

(defun haskell-find-first-cabal-file (dir)
  (car (directory-files dir t "\\.cabal$")))

(defun haskell-find-project-cabal-file (&optional directory)
  (let ((dir (or directory default-directory))
        cabal-file)
    (while (and (null (setq cabal-file (haskell-find-first-cabal-file dir)))
                (progn
                  (setq dir
                        (file-name-directory
                         (directory-file-name
                          (file-name-directory dir))))
                  ;; Give up if we are already at the root dir.
                  (not (string= "/" dir)))))
    cabal-file))

    (defun haskell-add-missing-package ()
      (save-excursion
        (goto-char (or compilation-filter-start (point-min)))
        (when (re-search-forward
               (concat "It is a member of the hidden package"
                       " `\\(.+?\\)-\\([0-9].+\\)'\\.") nil t)
          (let ((cabal-file (haskell-find-project-cabal-file))
                (package (match-string 1))
                (version (match-string 2)))
            (message "Found build depends: %s-%s" package version)
            (when cabal-file
              (message "Cabal file is %s" cabal-file)
              (with-current-buffer (find-file cabal-file)
                (goto-char (point-max))
                (when (re-search-backward "[Bb]uild-depends:" nil t)
                  (message "Found build depends")
                  (forward-paragraph)
                  (indent-according-to-mode)
                  (insert ", " package " >= " version))))))))

    (defcustom hoogle-binary-path
      (expand-file-name "~/Library/Haskell/bin/hoogle")
      "Path to the local 'hoogle' binary."
      :type 'file
      :group 'haskell)))

(use-package helm-config
  :init
  (progn
    (bind-key "C-c M-x" 'helm-M-x)
    (bind-key "C-h a" 'helm-apropos)
    (bind-key "M-s a" 'helm-do-grep)
    (bind-key "M-s b" 'helm-occur)
    (bind-key "M-s F" 'helm-for-files)

    (use-package helm-commands)
    (bind-key "C-h e a" 'td-helm-apropos)
    (bind-key "C-x M-!" 'helm-command-from-zsh)
    (bind-key "C-x f" 'helm-find-git-file)

    (use-package helm-descbinds
      :commands helm-descbinds
      :init
      (fset 'describe-bindings 'helm-descbinds))

    (bind-key "C-h b" 'helm-descbinds))

  :config
  (helm-match-plugin-mode t))

(use-package helm-css-scss)

(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))

(use-package highlight-indentation
  :bind ("C-M-S-s-i h" . highlight-indentation))

(use-package hilit-chg
  :bind ("M-o C" . highlight-changes-mode))

(use-package hl-line
  :bind ("M-o h" . hl-line-mode)
  :config
  (use-package hl-line+))

(use-package ido
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :init
  (ido-mode t)

  :config
  (progn
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode))))

(use-package ielm
  :bind ("C-c :" . ielm)
  :config
  (progn
    (defun td-ielm-return ()
      (interactive)
      (let ((end-of-sexp (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (point))))
        (if (>= (point) end-of-sexp)
            (progn
              (goto-char (point-max))
              (skip-chars-backward " \t\n\r")
              (delete-region (point) (point-max))
              (call-interactively #'ielm-return))
          (call-interactively #'paredit-newline))))

    (add-hook 'ielm-mode-hook
              (function
               (lambda ()
                 (bind-key "<return>" 'td-ielm-return ielm-map)))
              t)))

(use-package image-file
  :init
  (auto-image-file-mode 1))

(use-package impatient-mode
  :config
  (httpd-start))

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :init
  (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)

  :config
  (progn
    (defadvice info-setup (after load-info+ activate)
      (use-package info+))

    (defadvice Info-exit (after remove-info-window activate)
      "When info mode is quit, remove the window."
      (if (> (length (window-list)) 1)
          (delete-window)))))

(use-package info-look
  :commands info-lookup-add-help)

(setq Info-default-directory-list
      (apply #'append (list
                       (list "/Users/tonyday/.emacs.d/info")
                       (list "/usr/share/info"))))
(setq Info-directory-list Info-default-directory-list)

(use-package indirect
  :bind ("C-c C" . indirect-region))

(use-package jabber
  :init
  (progn
    (defvar jabber-user-id "45642_305487")
(defvar jabber-server-id "chat.hipchat.com")
    ;;(bind-key "C-M-S-s-j" '(jabber-connect jabber-user-id jabber-server-id))
))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :init
  (progn
    (use-package flycheck)
    (add-hook 'js2-mode-hook 'td-js2-hook)
    (defun td-js2-hook ()
      (flycheck-mode)
      (yas-minor-mode 1)
      (skewer-mode)
      (set (make-local-variable 'yas-fallback-behavior) 'call-other-command))))

(use-package keyfreq
  :init
  (progn
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

;; Utilities every Emacs Lisp coders should master:
;;
;;   paredit          Let's you manipulate sexps with ease
;;   redshank         Think: Lisp refactoring
;;   edebug           Knowing the traditional debugger is good too
;;   eldoc
;;   cldoc
;;   elint
;;   elp
;;   ert

(use-package lisp-mode
  ;; :load-path "site-lisp/slime/contrib/"
  :init
  (progn
    (defface esk-paren-face
      '((((class color) (background dark))
         (:foreground "grey50"))
        (((class color) (background light))
         (:foreground "grey55")))
      "Face used to dim parentheses."
      :group 'starter-kit-faces)

    ;; Change lambda to an actual lambda symbol
    (mapc (lambda (major-mode)
            (font-lock-add-keywords
             major-mode
             '(("(\\(lambda\\)\\>"
                (0 (ignore
                    (compose-region (match-beginning 1)
                                    (match-end 1) ?λ))))
               ("(\\|)" . 'esk-paren-face)
               ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
                (1 font-lock-keyword-face)
                (2 font-lock-function-name-face
                 nil t)))))
          lisp-modes)

    (defvar slime-mode nil)
    (defvar lisp-mode-initialized nil)

    (defun initialize-lisp-mode ()
      (unless lisp-mode-initialized
        (setq lisp-mode-initialized t)

        (use-package redshank
          :diminish redshank-mode)

        (use-package elisp-slime-nav
          :diminish elisp-slime-nav-mode)

        (use-package edebug)

        (use-package eldoc
          :diminish eldoc-mode
          :defer t
          :init
          (use-package eldoc-extension
            :disabled t
            :defer t
            :init
            (add-hook 'emacs-lisp-mode-hook
                      #'(lambda () (require 'eldoc-extension)) t))

          :config
          (eldoc-add-command 'paredit-backward-delete
                             'paredit-close-round))

        (use-package ert
          :commands ert-run-tests-interactively
          :bind ("C-c e t" . ert-run-tests-interactively))

        (use-package elint
          :commands 'elint-initialize
          :init
          (defun elint-current-buffer ()
            (interactive)
            (elint-initialize)
            (elint-current-buffer))

          :config
          (progn
            (add-to-list 'elint-standard-variables 'current-prefix-arg)
            (add-to-list 'elint-standard-variables 'command-line-args-left)
            (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
            (add-to-list 'elint-standard-variables 'emacs-major-version)
            (add-to-list 'elint-standard-variables 'window-system)))

        (defun td-elisp-indent-or-complete (&optional arg)
          (interactive "p")
          (call-interactively 'lisp-indent-line)
          (unless (or (looking-back "^\\s-*")
                      (bolp)
                      (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
            (call-interactively 'lisp-complete-symbol)))

        (defun td-lisp-indent-or-complete (&optional arg)
          (interactive "p")
          (if (or (looking-back "^\\s-*") (bolp))
              (call-interactively 'lisp-indent-line)
            (call-interactively 'slime-indent-and-complete-symbol)))

        (defun td-byte-recompile-file ()
          (save-excursion
            (byte-recompile-file buffer-file-name)))

        ;; Register Info manuals related to Lisp
        (use-package info-lookmore
          :init
          (progn
            (info-lookmore-elisp-cl)
            (info-lookmore-elisp-userlast)
            (info-lookmore-elisp-gnus)
            (info-lookmore-apropos-elisp)))

        (mapc (lambda (mode)
                (info-lookup-add-help
                 :mode mode
                 :regexp "[^][()'\" \t\n]+"
                 :ignore-case t
                 :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
              lisp-modes)))

    (defun td-lisp-mode-hook ()
      (initialize-lisp-mode)

      (auto-fill-mode 1)
      (paredit-mode 1)
      (redshank-mode 1)
      (elisp-slime-nav-mode 1)

      (local-set-key (kbd "<return>") 'paredit-newline)

      (if (memq major-mode
                '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
          (progn
            (bind-key "<M-return>" 'outline-insert-heading emacs-lisp-mode-map)
            (bind-key "<tab>" 'td-elisp-indent-or-complete emacs-lisp-mode-map))
        (turn-on-eldoc-mode)

        (bind-key "<tab>" 'td-lisp-indent-or-complete lisp-mode-map)
        (bind-key "M-q" 'slime-reindent-defun lisp-mode-map)
        (bind-key "M-l" 'slime-selector lisp-mode-map))

      (yas-minor-mode 1))

    (hook-into-modes #'td-lisp-mode-hook lisp-mode-hooks)))

(use-package llvm-mode
  :mode ("\\.ll\\'" . llvm-mode))

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :bind ("C-x g" . magit-status)
  :defer t
  :config
  (progn
    (setenv "GIT_PAGER" "")

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)))

    (use-package magit-topgit)
    (use-package rebase-mode)
    (use-package magithub)
    (use-package org-magit)))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (progn
    (defun markdown-preview-file ()
      "run Marked on the current file and revert the buffer"
      (interactive)
      (shell-command
       (format "open -a /Applications/Marked.app %s"
               (shell-quote-argument (buffer-file-name)))))

    (bind-key "C-x M" 'markdown-preview-file)))

(use-package miniedit
  :commands minibuffer-edit
  :init
  (progn
    (bind-key "\M-\C-e" 'miniedit minibuffer-local-map)
    (bind-key "\M-\C-e" 'miniedit minibuffer-local-ns-map)
    (bind-key "\M-\C-e" 'miniedit minibuffer-local-completion-map)
    (bind-key "\M-\C-e" 'miniedit minibuffer-local-must-match-map)))

(use-package mule
  :init
  (progn
    (prefer-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-S-s-m" . mc/edit-lines)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :init
  (setq mc/list-file (expand-file-name "~/.emacs.d/data/mc-lists.el")))

(use-package o-blog
  :commands (o-blog-tangle
             o-blog-publish
             o-blog-view
             o-blog-local
             o-blog-produce)
  :config
  (progn
    (defvar o-blog-local-site "~/Sites/dev")
    (defvar o-blog-out-dir "~/git/tonyday567.github.com")
    (defvar o-blog-local-url-index "http://127.0.0.1/~tonyday/dev/index.html")
    (defvar github-site-dir "~/git/tonyday567.github.com")

    (defun o-blog-tangle ()
      (interactive)
      "tangle style and template files, and convert less style file to css"
      (org-babel-tangle nil nil "css")
      (org-babel-tangle nil nil "html")
      (eshell-command "lessc style/less/scarce.less style/css/scarce.css"))

    (defun o-blog-publish ()
      (interactive)
      "convert to site pages"
      (org-publish-blog buffer-file-name))

    (defun o-blog-local ()
      (interactive)
      "publish pages locally"
      (if (file-exists-p o-blog-local-site)
          (delete-directory o-blog-local-site t))
      (copy-directory
       (format "%s" o-blog-out-dir) o-blog-local-site))

    (defun o-blog-view ()
      (interactive)
      "view index.html file in browser"
      (browse-url o-blog-local-url-index))

    (defun o-blog-push ()
      (interactive)
      "copy o-blog out directory to repo"
      (copy-directory
       (format "%s%s" default-directory o-blog-out-dir)
       github-site-dir nil t t))

    (defun o-blog-produce ()
      (interactive)
      "tangle, publish blog locally, then push to local repo"
      (o-blog-tangle)
      (o-blog-publish)
      (o-blog-push))

    (defun ob-get-posts-by-property (BLOG PROP VAL)
      "Returns a list of valid posts with the property PROP equal to VAL"
      (with-current-buffer (ob:blog-buffer BLOG)

        (let ((headings nil))
          (org-map-entries
           `(if (string= VAL (org-entry-get (point) PROP))
                (setq headings (cons (org-get-heading t t) headings)))
           t
           'file)
          (setq headings (delq nil headings))
          (ob:get-posts
           (lambda (x)
             (cl-some
              (lambda (z)
                (string= z (ob:post-title x)))
              headings))
           4))))


    (defun ob-get-post-by-title (POSTS TITLE)
      "Returns a valid post with the title TITLE or null"
      (let ((POST-LIST POSTS))
        (while (and POSTS
                    (not (string= (ob:post-title (car POST-LIST)) TITLE)))
          (setq POST-LIST (cdr POST-LIST)))
        (car POST-LIST))))


  :bind (("C-c c t" . o-blog-tangle)
         ("C-c c p" . o-blog-publish)
         ("C-c c l" . o-blog-local)
         ("C-c c v" . o-blog-view)
         ("C-c c u" . o-blog-push)
         ("C-c c ." . o-blog-produce)))

(bind-key "C-. j" 'org-jump)

(bind-key "C-c a  " 'org-agenda              )
(bind-key "C-c r  " 'org-capture             )
(bind-key "C-c l  " 'org-store-link          )
(bind-key "C-c b  " 'org-iswitchb            )
(bind-key "C-M-S-s-c w  " 'td-widen                )
(bind-key "C-M-S-s-c n  " 'td-narrow-to-subtree    )
(bind-key "C-M-S-s-c u  " 'td-narrow-up-one-level  )
(bind-key "C-c l  " 'org-store-link          )
(bind-key "C-M-S-s-o c  " 'org-cycle-agenda-files  )
(bind-key "C-. C-c" 'org-indent-indent-buffer)

;;(load "org-settings")
(use-package org-colview)
(use-package org-special-blocks)
(use-package org-capture)
(use-package org-crypt)
(use-package org-indent
  :diminish org-indent-mode)
(use-package org-checklist
  :load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp/")

(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(use-package bookmark+)

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
(defun td-org-add-yas-hook ()
  (require 'yasnippet)
  (yas-minor-mode 1)
  (set (make-local-variable 'yas-trigger-key) [tab])
  (set (make-local-variable 'yas-fallback-behavior) nil)
  (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand))
  ;;(define-key yas-keymap [tab] 'yas-next-field-or-maybe-expand))
(add-hook 'org-mode-hook 'td-org-add-yas-hook)

(defun org-jump ()
  (interactive)
  (bookmark-set "org-jumped-from")
  (org-refile t nil nil "Jump")
  (bookmark-set "org-jumped-to"))


(defun org-jump-back()
  (interactive)
  (if (equal (point) (bookmark-get-position "org-jumped-from"))
      (bookmark-jump "org-jumped-to")
    (if (bookmark-get-position "org-jumped-to")
        (bookmark-jump "org-jumped-from"))))

(bind-key "C-. j" 'org-jump)
(bind-key "C-. l" 'org-jump-back)

(defun org-remove-results ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "^#\\+results:.*
\\(^\\|.+
\\)*
" nil t)
      (replace-match ""))))

(bind-key "C-c t r" 'org-remove-results)

(defun org-random-entry (&optional arg)
  "Select and goto a random todo item from the global agenda"
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg org-agenda-overriding-arguments))
  (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
  (let* ((today (org-today))
         (date (calendar-gregorian-from-absolute today))
         (kwds org-todo-keywords-for-agenda)
         (lucky-entry nil)
         (completion-ignore-case t)
         (org-select-this-todo-keyword
          (if (stringp arg) arg
            (and arg (integerp arg) (> arg 0)
                 (nth (1- arg) kwds))))
         rtn rtnall files file pos marker buffer)
    (when (equal arg '(4))
      (setq org-select-this-todo-keyword
            (org-icompleting-read "Keyword (or KWD1|K2D2|...): "
                                  (mapcar 'list kwds) nil nil)))
    (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
    (catch 'exit
      (org-compile-prefix-format 'todo)
      (org-set-sorting-strategy 'todo)
      (setq files (org-agenda-files nil 'ifmode)
            rtnall nil)
      (while (setq file (pop files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (setq rtn (org-agenda-get-day-entries file date :todo))
          (setq rtnall (append rtnall rtn))))

      (when rtnall
        (setq lucky-entry
              (nth (random
                    (safe-length
                     (setq entries rtnall)))
                   entries))

        (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                         (org-agenda-error)))
        (setq buffer (marker-buffer marker))
        (setq pos (marker-position marker))
        (org-pop-to-buffer-same-window buffer)
        (widen)
        (goto-char pos)
        (when (derived-mode-p 'org-mode)
          (org-show-context 'agenda)
          (save-excursion
            (and (outline-next-heading)
                 (org-flag-heading nil))) ; show the next heading
          (when (outline-invisible-p)
            (show-entry))                 ; display invisible text
          (run-hooks 'org-agenda-after-show-hook))))))

(bind-key "C-. C-. r" 'org-random-entry)

(bind-key "C-M-S-s-l" 'omlg-grab-link)

(defun org-scratch ()
  (interactive)
  (if (not (bufferp "scratch.org"))
      (find-file "~/.emacs.d/tmp/scratch.org")
    (switch-to-buffer "scratch.org")))

(use-package ox-latex
:config
  (progn
  (add-to-list
   'org-latex-classes
   '("scarce-org-article"
     "\\documentclass[12pt]{article}
      % miscellaneous and unknown uses
      \\usepackage{amssymb}
      \\usepackage{graphicx}
      \\usepackage{hyperref}
      \\usepackage[parfill]{parskip}
      \\usepackage{paralist}

      % font specifications
      \\usepackage[T1]{fontenc}
      \\usepackage{fontspec,xunicode}
      \\defaultfontfeatures{Mapping=tex-text}
      \\setromanfont[Mapping=tex-text]{Droid Sans}
      \\setsansfont[Scale=MatchLowercase,Mapping=tex-text]{Droid Serif}
      \\setmonofont[Scale=MatchLowercase]{Droid Sans Mono}

      % geometry
      \\usepackage{geometry}
      \\geometry{a4paper, textwidth=6.5in, textheight=10in,
      marginparsep=7pt, marginparwidth=.6in}
      \\pagestyle{plain}
      \\title{}

      % color
      \\usepackage{color}
      %\\definecolor{greytext}{rgb}{0.6,0.6,0.6}
      %\\definecolor{greyline}{rgb}{0.8,0.8,0.8}
      \\definecolor{greytext}{gray}{0.3}
      \\definecolor{greyline}{gray}{0.8}

      \\renewcommand{\\labelitemi}{
      \\color{greytext}\\scalebox{1}{$-$}}

      % custom maketitle
      \\makeatletter
      \\renewcommand{\\@maketitle}{
      \\null
      \\vskip 0em%
      \\begin{flushleft}%
      {\\fontfamily{\\sfdefault}\\selectfont
      {\\LARGE \\@title \\par}%
      \\vskip 1em
      {\\Large \\@author \\par}
      \\vskip 1em
      {\\@date \\par}
      }
      \\end{flushleft}%
      \\par}
      \\makeatother

      % style
      \\usepackage{enumitem}
      \\setitemize[0]{leftmargin=*}
      % \\usepackage[none]{hyphenat}%%%%
      % \\usepackage[document]{ragged2e}

      \\usepackage{transparent}

      % a fancy header
      \\usepackage{fancyhdr}
      %\\renewcommand{\\headheight}{0.6in}
      \\setlength{\\headheight}{1.6cm}
      \\setlength{\\headwidth}{\\textwidth}
      \\renewcommand{\\headrulewidth}{2pt}
      \\renewcommand{\\headrule}{\\hbox to\\headwidth{%
      \\color{greyline}\\leaders\\hrule height \\headrulewidth\\hfill}}

      \\fancypagestyle{plain}{
      \\fancyhead[R]{}% empty left
      \\fancyhead[L]{ % right
      \\includegraphics[height=1cm]{/Users/tonyday/stuff/site/assets/logo-grey.pdf}
      }
      \\fancyfoot[C]{}
      %\\fancyhf{}
      }

      % end spec lines.  The rest is org generated

      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     (" \\section{%s}" . "\\section*{%s}")
     (" \\subsection{%s}" . "\\subsection*{%s}")
     (" \\subsubsection{%s}" . "\\subsubsection*{%s}")
     (" \\paragraph{%s}" . "\\paragraph*{%s}")
     (" \\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(use-package ox-md)

(defun td-widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'td-widen))
          'append)

(defun td-narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun td-narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (td-narrow-to-org-subtree))
    (td-narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'td-narrow-to-subtree))
          'append)

(defun td-narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (td-narrow-to-org-subtree)))

(defun td-get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun td-narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (td-get-pom-from-agenda-restriction-or-point)
        (td-narrow-up-one-org-level))
    (td-narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'td-narrow-up-one-level))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'td-set-agenda-restriction-lock))
          'append)

(defun td-set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (td-get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type)))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

(defun org-fit-agenda-window ()
  "Fit the window to the buffer size."
  (and (memq org-agenda-window-setup '(reorganize-frame))
       (fboundp 'fit-window-to-buffer)
       (fit-window-to-buffer)))

(defun org-agenda-jump ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (when (called-interactively-p 'any)
              (select-window wind)
              (org-fit-window-to-buffer))
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (call-interactively 'org-agenda-list))))

      (bind-key "C-. a" 'org-agenda-jump)

(use-package org-trello)

(use-package pandoc-mode
  :init (add-hook 'markdown-mode-hook 'turn-on-pandoc))

(use-package paredit
  :commands paredit-mode
  :diminish paredit-mode
  :config
  (progn
    (use-package paredit-ext)

    (bind-key "C-M-l" 'paredit-recentre-on-sexp paredit-mode-map)

    (bind-key ")" 'paredit-close-round-and-newline paredit-mode-map)
    (bind-key "M-)" 'paredit-close-round paredit-mode-map)

    (bind-key "M-k" 'paredit-raise-sexp paredit-mode-map)
    (bind-key "M-h" 'mark-containing-sexp paredit-mode-map)
    (bind-key "M-I" 'paredit-splice-sexp paredit-mode-map)

    (unbind-key "M-r" paredit-mode-map)
    (unbind-key "M-s" paredit-mode-map)

    (bind-key "C-. d" 'paredit-forward-down paredit-mode-map)
    (bind-key "C-. B" 'paredit-splice-sexp-killing-backward paredit-mode-map)
    (bind-key "C-. C" 'paredit-convolute-sexp paredit-mode-map)
    (bind-key "C-. F" 'paredit-splice-sexp-killing-forward paredit-mode-map)
    (bind-key "C-. a" 'paredit-add-to-next-list paredit-mode-map)
    (bind-key "C-. A" 'paredit-add-to-previous-list paredit-mode-map)
    (bind-key "C-. j" 'paredit-join-with-next-list paredit-mode-map)
    (bind-key "C-. J" 'paredit-join-with-previous-list paredit-mode-map)

    (add-hook 'allout-mode-hook
              #'(lambda ()
                  (bind-key "M-k" 'paredit-raise-sexp allout-mode-map)
                  (bind-key "M-h" 'mark-containing-sexp allout-mode-map)))))

(unless
    (use-package mic-paren
          :init
          (paren-activate))

  (use-package paren
    :init
    (show-paren-mode 1)))

(use-package per-window-point
  :init
  (pwp-mode 1))

(use-package persistent-scratch)

(use-package popup-ruler
  :bind (("C-. r" . popup-ruler)
         ("C-. R" . popup-ruler-vertical)))

(use-package pp-c-l
  :init
  (hook-into-modes 'pretty-control-l-mode '(prog-mode-hook)))

(use-package proof-site
  :load-path "site-lisp/proofgeneral/generic/"
  :config
  (progn
    (eval-after-load "coq"
      '(progn
         (add-hook 'coq-mode-hook
                   (lambda ()
                     (yas-minor-mode 1)
                     (whitespace-mode 1)
                     (unicode-tokens-use-shortcuts 0)))
         (bind-key "M-RET" 'proof-goto-point coq-mode-map)
         (bind-key "<tab>" 'yas-expand-from-trigger-key coq-mode-map)
         (bind-key "C-c C-p" (lambda ()
                               (interactive)
                               (proof-layout-windows)
                               (proof-prf)) coq-mode-map)))))

(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-global-mode))

(use-package recentf
  :if (not noninteractive)
  :init
  (progn
    (recentf-mode 1)

    (defun recentf-add-dired-directory ()
      (if (and dired-directory
               (file-directory-p dired-directory)
               (not (string= "/" dired-directory)))
          (let ((last-idx (1- (length dired-directory))))
            (recentf-add-file
             (if (= ?/ (aref dired-directory last-idx))
                 (substring dired-directory 0 last-idx)
               dired-directory)))))

    (add-hook 'dired-mode-hook 'recentf-add-dired-directory)))

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :config
  (progn
    (use-package yari
      :init
      (progn
        (defvar yari-helm-source-ri-pages
          '((name . "RI documentation")
            (candidates . (lambda () (yari-ruby-obarray)))
            (action  ("Show with Yari" . yari))
            (candidate-number-limit . 300)
            (requires-pattern . 2)
            "Source for completing RI documentation."))

        (defun helm-yari (&optional rehash)
          (interactive (list current-prefix-arg))
          (when current-prefix-arg (yari-ruby-obarray rehash))
          (helm 'yari-helm-source-ri-pages (yari-symbol-at-point)))))

    (defun td-ruby-smart-return ()
      (interactive)
      (when (memq (char-after) '(?\| ?\" ?\'))
        (forward-char))
      (call-interactively 'newline-and-indent))

    (defun td-ruby-mode-hook ()
      (require 'inf-ruby)
      (inf-ruby-keys)

      (bind-key "<return>" 'td-ruby-smart-return ruby-mode-map)
      (bind-key "C-h C-i" 'helm-yari ruby-mode-map)

      (set (make-local-variable 'yas/fallback-behavior)
           '(apply ruby-indent-command . nil))
      (bind-key "<tab>" 'yas/expand-from-trigger-key ruby-mode-map))

    (add-hook 'ruby-mode-hook 'td-ruby-mode-hook)))

(use-package scss-mode
  ;;:init (add-hook 'markdown-mode-hook 'turn-on-pandoc)
  :mode ("\\.scss\\'" . scss-mode))

(use-package simple-httpd
  :config (httpd-start))

(use-package skewer-mode
  ;;:commands (run-skewer skewer-mode)
  :bind (("C-M-S-s-s r" . run-skewer)
         ("C-M-S-s-s s" . skewer-mode)
         ("C-M-S-s-s i" . skewer-repl))

  :config
  (progn
    (use-package skewer-repl)
    (define-prefix-command 'skewer-mode-map)
    (bind-key "C-c C-f" 'flycheck-buffer skewer-mode-map)
    (bind-key "C-c C-l" 'skewer-load-buffer skewer-mode-map)
    (bind-key "C-c C-z" 'skewer-repl skewer-mode-map)
    (bind-key "C-M-x" 'skewer-eval-defun skewer-mode-map)
    (bind-key "C-c C-p" 'skewer-eval-print-last-expression skewer-mode-map)
    (bind-key "C-x C-e" 'skewer-eval-last-expression skewer-mode-map)
    (bind-key "C-c C-`" 'list-skewer-clients skewer-mode-map)
    (bind-key "C-c C-L" 'skewer-coffee-load-buffer skewer-mode-map)
    (bind-key "M-p" 'flycheck-previous-error skewer-mode-map)
    (bind-key "M-n" 'flycheck-next-error skewer-mode-map)

    (defun skewer-coffee-load-buffer ()
      (interactive)
      (coffee-compile-buffer)
      (switch-to-buffer coffee-compiled-buffer-name)
      (skewer-load-buffer)
      (kill-buffer coffee-compiled-buffer-name))

    ;; ridiculously run this again to get the edited keymap
    (define-minor-mode skewer-mode
      "Minor mode for interacting with a browser."
      :lighter " skewer"
      :keymap skewer-mode-map
      :group 'skewer)))

(use-package smart-compile
  :commands smart-compile
  :bind (("C-c !" . smart-compile)
         ("A-n"   . next-error)
         ("A-p"   . previous-error))
  :init
  (progn
    (defun show-compilation ()
      (interactive)
      (let ((compile-buf
             (catch 'found
               (dolist (buf (buffer-list))
                 (if (string-match "\\*compilation\\*" (buffer-name buf))
                     (throw 'found buf))))))
        (if compile-buf
            (switch-to-buffer-other-window compile-buf)
          (call-interactively 'compile))))

  (bind-key "M-O" 'show-compilation)))

(use-package smart-forward
  :bind (("C-<up>" . smart-up)
         ("C-<down>" . smart-down)
         ("C-<left>" . smart-backward)
         ("C-<right>" . smart-forward)))

(use-package smex)

(use-package speedbar
  :config   (speedbar-add-supported-extension ".hs"))

(use-package stopwatch
  :bind ("<f8>" . stopwatch))

(use-package shm
  :load-path "~/.emacs.d/site-lisp/structured-haskell-mode/elisp/")

(use-package texinfo
  :defines texinfo-section-list
  :mode ("\\.texi\\'" . texinfo-mode)
  :config
  (progn
    (defun td-texinfo-mode-hook ()
      (dolist (mapping '((?b . "emph")
                         (?c . "code")
                         (?s . "samp")
                         (?d . "dfn")
                         (?o . "option")
                         (?x . "pxref")))
        (local-set-key (vector (list 'alt (car mapping)))
                       `(lambda () (interactive)
                          (TeX-insert-macro ,(cdr mapping))))))

    (add-hook 'texinfo-mode-hook 'td-texinfo-mode-hook)

    (defun texinfo-outline-level ()
      ;; Calculate level of current texinfo outline heading.
      (require 'texinfo)
      (save-excursion
        (if (bobp)
            0
          (forward-char 1)
          (let* ((word (buffer-substring-no-properties
                        (point) (progn (forward-word 1) (point))))
                 (entry (assoc word texinfo-section-list)))
            (if entry
                (nth 1 entry)
              5)))))))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

(use-package vkill
  :commands vkill
  :init
  (progn
    (defun vkill-and-helm-occur ()
      (interactive)
      (vkill)
      (call-interactively #'helm-occur))

    (bind-key "C-x L" 'vkill-and-helm-occur))

  :config
  (setq vkill-show-all-processes t))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :init
  (progn
    (hook-into-modes 'whitespace-mode
                     '(prog-mode-hook
                       c-mode-common-hook))

    (defun normalize-file ()
      (interactive)
      (save-excursion
      (goto-char (point-min))
        (whitespace-cleanup)
        (delete-trailing-whitespace)
        (goto-char (point-max))
        (delete-blank-lines)
        (set-buffer-file-coding-system 'unix)
        (goto-char (point-min))
        (while (re-search-forward "\r$" nil t)
          (replace-match ""))
        (set-buffer-file-coding-system 'utf-8)
        (let ((require-final-newline t))
          (save-buffer))))

    (defun maybe-turn-on-whitespace ()
      "Depending on the file, maybe clean up whitespace."
      (let ((file (expand-file-name ".clean"))
            parent-dir)
        (while (and (not (file-exists-p file))
                    (progn
                      (setq parent-dir
                            (file-name-directory
                             (directory-file-name
                              (file-name-directory file))))
                      ;; Give up if we are already at the root dir.
                      (not (string= (file-name-directory file)
                                    parent-dir))))
          ;; Move up to the parent dir and try again.
          (setq file (expand-file-name ".clean" parent-dir)))
        ;; If we found a change log in a parent, use that.
        (when (and (file-exists-p file)
                   (not (file-exists-p ".noclean"))
                   (not (and buffer-file-name
                             (string-match "\\.texi\\'" buffer-file-name))))
          (add-hook 'write-contents-hooks
                    #'(lambda ()
                        (ignore (whitespace-cleanup))) nil t)
          (whitespace-cleanup))))

    (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t))

  :config
  (progn
    (remove-hook 'find-file-hooks 'whitespace-buffer)
    (remove-hook 'kill-buffer-hook 'whitespace-buffer)))

(use-package winner
  :diminish winner-mode
  :if (not noninteractive)
  :init
  (progn
    (winner-mode 1)

    (bind-key "M-N" 'winner-redo)
    (bind-key "M-P" 'winner-undo)))

(use-package yasnippet
  :if (not noninteractive)
  :commands (yas-minor-mode
             yas-expand
             yas-trigger-key
             yas-org-very-safe-expand
             yas-next-field-or-maybe-expand)
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (progn
    (defun yas-org-very-safe-expand ()
      (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
    (hook-into-modes #'(lambda () (yas-minor-mode 1))
                     '(prog-mode-hook
                       ruby-mode-hook
                       message-mode-hook
                       gud-mode-hook
                       erc-mode-hook)))
  :config
  (progn
    (setq yas-load-directory (expand-file-name "snippets/" user-emacs-directory))

    (bind-key "<tab>" 'yas-next-field-or-maybe-expand yas-keymap)


    (defun yas-new-snippet (&optional choose-instead-of-guess)
      (interactive "P")
      (let ((guessed-directories (yas--guess-snippet-directories)))
        (switch-to-buffer "*new snippet*")
        (erase-buffer)
        (kill-all-local-variables)
        (snippet-mode)
        (set (make-local-variable 'yas--guessed-modes)
             (mapcar #'(lambda (d)
                         (intern (yas--table-name (car d))))
                     guessed-directories))
        (unless (and choose-instead-of-guess
                     (not (y-or-n-p "Insert a snippet with useful headers? ")))
          (yas-expand-snippet "\
# -*- mode: snippet -*-
# name: $1
# --
$0"))))
    (defvar ctl-c-y-map)
    (define-prefix-command 'ctl-c-y-map)
    (bind-key "C-c y" 'ctl-c-y-map)
    (bind-key "C-c y TAB" 'yas-expand)
    (bind-key "C-c y n" 'yas-new-snippet)
    (bind-key "C-c y f" 'yas-find-snippets)
    (bind-key "C-c y r" 'yas-reload-all)
    (bind-key "C-c y v" 'yas-visit-snippet-file)))

(use-package zencoding-mode
  :commands zencoding-mode
  :init
  (progn
    (add-hook 'nxml-mode-hook 'zencoding-mode)
    (add-hook 'html-mode-hook 'zencoding-mode)
    (add-hook 'html-mode-hook
              #'(lambda ()
                (bind-key "<return>" 'newline-and-indent html-mode-map))))

  :config
  (progn
    (defvar zencoding-mode-keymap (make-sparse-keymap))
    (bind-key "C-c C-c" 'zencoding-expand-line zencoding-mode-keymap)
    (unbind-key "C-<return>" zencoding-mode-keymap)))

(setq custom-file (expand-file-name "settings.el" user-emacs-directory))
(load custom-file)

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
