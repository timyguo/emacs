(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-scroll-prefer-subnodes t)
 '(LaTeX-command "latex -synctex=1")
 '(TeX-auto-save t)
 '(TeX-command-list (quote (("XeLaTeX_SyncteX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run XeLaTeX") ("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function nil t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command") ("Jump to PDF" "%V" TeX-run-discard-or-function nil t :help "Run Viewer"))))
 '(TeX-master t)
 '(TeX-modes (quote (tex-mode plain-tex-mode texinfo-mode latex-mode doctex-mode)))
 '(TeX-parse-self t)
 '(TeX-view-program-list (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b") ("Preview" "open -a Preview.app %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Skim") (output-html "xdg-open"))))
 '(alert-default-style (quote growl))
 '(alert-fade-time 1)
 '(alert-growl-command "/usr/local/bin/growlnotify")
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(bbdb-default-country "")
 '(bbdb-file "~/Documents/bbdb")
 '(bbdb-message-caching-enabled nil)
 '(bbdb-no-duplicates t)
 '(bbdb-offer-save (quote savenoprompt))
 '(bbdb-silent-running t)
 '(bbdb-use-pop-up nil)
 '(bbdb-vcard-import-translation-table (quote (("CELL\\|CAR" . "Mobile") ("WORK" . "Work") ("HOME" . "Home") ("^$" . "Work"))))
 '(bbdb/mail-auto-create-p nil)
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-kill-ring-quit-action (quote save-and-restore))
 '(clean-buffer-list-kill-never-buffer-names (quote ("*scratch*" "*Messages*" "*server*" "*Group*" "*Org Agenda*" "todo.txt" "&bitlbee")))
 '(clean-buffer-list-kill-never-regexps (quote ("^ \\*Minibuf-.*\\*$" "^\\*Summary" "^\\*Article" "^#")))
 '(clean-buffer-list-kill-regexps (quote (".*")))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(custom-file "/Users/tonyday/.emacs.d/settings.el")
 '(custom-safe-themes (quote ("0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "1b7caa779ad718320632954165993f48d9a0418efb59e131d53f3b952f83bde3" "c34923d2626e38a71679f6d3c9d27b01dcff57e249e399fc727528ca0533f1d3" "fcbdc19b1b8adf59c3fc41b5030ed5b6af8c40564be4fa90c56971e4cefb96c9" "951e10f17de57de1e0c9cbeb44fcdda1b6c6d26beab40c3bd0abbfc38dd5c9c8" "1f213419f5950f893b774696c6b62bb942f7e61138e0c9f1f22ef8525cf8b23e" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(custom-theme-load-path (quote (custom-theme-directory t "/Users/tonyday/.emacs.d/elpa/zenburn-theme-1.5")))
 '(default-frame-alist (quote ((font . "Monaco-12") (cursor-color . "#cabb59"))))
 '(default-input-method "latin-1-prefix")
 '(default-major-mode (quote text-mode) t)
 '(delete-selection-mode nil)
 '(diary-file "~/Documents/diary")
 '(diff-default-read-only t nil nil "
If you don't do this, all the nice navigation stuff is disabled by
default.  Who wants to edit diffs by hand, anyway?")
 '(diff-jump-to-old-file t)
 '(diff-mode-hook (quote (diff-delete-empty-files diff-make-unified smerge-mode)))
 '(diff-switches "-du")
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t nil nil "This customization replaces John's entire desire for sunrise,
which I now deinstall with relish")
 '(dired-listing-switches "-alh" nil nil "
Added -h so I can read file sizes")
 '(dired-no-confirm (quote (byte-compile chgrp chmod chown copy hardlink symlink touch)))
 '(dired-omit-files "^\\.?#\\|^\\.\\(DS_Store\\|localized\\|AppleDouble\\)$\\|^\\.\\.$")
 '(dired-omit-mode nil t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(ede-project-directories (quote ("/Users/tonyday/projects/tpoker/include" "/Users/tonyday/projects/tpoker/src" "/Users/tonyday/projects/tpoker")))
 '(edebug-sit-for-seconds 0.1)
 '(ediff-combination-pattern (quote ("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming")))
 '(ediff-custom-diff-options "-u" nil nil "
Show me unified diffs by default")
 '(ediff-diff-options "-d")
 '(ediff-highlight-all-diffs nil nil nil "
only highlight the selected diff (keeps down gray cruft onscreen)")
 '(ediff-keep-variants nil nil nil "
Any unchanged buffers in the ediff are removed when the session ends. 
`C-u q' to override when quitting.")
 '(ediff-merge-filename-prefix "")
 '(ediff-show-clashes-only t)
 '(ediff-skip-merge-regions-that-differ-from-default nil)
 '(ediff-split-window-function (quote split-window-horizontally) nil nil "
Show diffs side-by-side")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain) nil nil "
Run Ediff all in one frame.  The default when there's a window manager is for
emacs to pop up a separate frame for the `*Ediff Control Panel*' buffer")
 '(edit-server-new-frame nil)
 '(erc-auto-query (quote window-noselect))
 '(erc-autojoin-channels-alist (quote (("localhost" "&bitlbee") ("freenode.net" "#haskell" "#freenode" "#org-mode" "#emacs"))))
 '(erc-autojoin-mode t)
 '(erc-hide-list (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE")))
 '(erc-keyword-highlight-type (quote all))
 '(erc-keywords (quote ("emacs")))
 '(erc-log-channels-directory "~/Messages/ERC")
 '(erc-log-write-after-send t)
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom services stamp track)))
 '(erc-nick "tonyday567")
 '(erc-port 6667)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-server "irc.freenode.net")
 '(erc-services-mode t)
 '(erc-text-matched-hook (quote (erc-log-matches erc-hide-fools)))
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude-types (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(erc-track-faces-priority-list (quote (erc-error-face (erc-nick-default-face erc-current-nick-face) erc-current-nick-face erc-keyword-face (erc-nick-default-face erc-pal-face) erc-pal-face erc-nick-msg-face erc-direct-msg-face)))
 '(erc-user-full-name (quote user-full-name))
 '(erc-warn-about-blank-lines nil)
 '(eshell-directory-name "~/.emacs.d/eshell/")
 '(eshell-history-size 1000)
 '(eshell-ls-dired-initial-args (quote ("-h")))
 '(eshell-ls-exclude-regexp "~\\'")
 '(eshell-ls-initial-args "-h")
 '(eshell-modules-list (quote (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prefer-to-shell t nil (eshell))
 '(eshell-prompt-function (lambda nil (concat (abbreviate-file-name (eshell/pwd)) (if (= (user-uid) 0) " # " " $ "))))
 '(eshell-save-history-on-exit t)
 '(eshell-stringify-t nil)
 '(eshell-term-name "ansi")
 '(eshell-visual-commands (quote ("vi" "top" "screen" "less" "lynx" "rlogin" "telnet")))
 '(ess-R-font-lock-keywords (quote ((ess-R-fl-keyword:modifiers . t) (ess-R-fl-keyword:fun-defs . t) (ess-R-fl-keyword:keywords . t) (ess-R-fl-keyword:assign-ops . t) (ess-R-fl-keyword:constants . t) (ess-fl-keyword:fun-calls . t) (ess-fl-keyword:numbers . t) (ess-fl-keyword:operators . t) (ess-fl-keyword:delimiters . t) (ess-fl-keyword:= . t) (ess-R-fl-keyword:F&T . t))))
 '(ess-S-assign "_")
 '(ess-ask-for-ess-directory nil)
 '(ess-eval-visibly nil)
 '(ess-history-directory "/Users/tonyday/.emacs.d/data")
 '(eval-expr-print-function (quote pp))
 '(fci-rule-color "#383838")
 '(fill-column 78)
 '(find-ls-subdir-switches "-alh")
 '(flycheck-checkers (quote (bash c/c++-clang c/c++-cppcheck coffee-coffeelint css-csslint elixir emacs-lisp emacs-lisp-checkdoc erlang go-gofmt go-build go-test haml haskell-ghc haskell-hdevtools haskell-hlint html-tidy javascript-jshint json-jsonlint less lua perl php php-phpcs puppet-parser puppet-lint python-flake8 python-pylint rst ruby-rubocop ruby ruby-jruby rust sass scala scss sh-dash sh-bash tex-chktex tex-lacheck xml-xmlstarlet zsh)))
 '(flymake-allowed-file-name-masks (quote ((".+\\.html$" flymake-html-validator-init flymake-simple-cleanup flymake-get-real-file-name) ("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-no-changes-timeout 5)
 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-issue-message-flag nil)
 '(flyspell-use-meta-tab nil)
 '(frame-title-format (quote (:eval (concat (if buffer-file-name default-directory "%b") "    " (number-to-string (cdr (assq (quote width) (frame-parameters)))) "x" (number-to-string (cdr (assq (quote height) (frame-parameters))))))) t)
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-config-use-unicode-symbols t)
 '(haskell-doc-chop-off-context nil)
 '(haskell-doc-show-global-types t)
 '(haskell-fay-buffer "*fay*")
 '(haskell-hoogle-command nil)
 '(haskell-indentation-cycle-warn nil)
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-starter-offset 0)
 '(haskell-mode-message-line-multi t)
 '(haskell-notify-p t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-program-name "ghci -i./")
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(hi2-show-indentations nil)
 '(hippie-expand-try-functions-list (quote (yas/hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length 200)
 '(hoogle-binary-path "/Users/tonyday/Library/haskell/bin/hoogle")
 '(html-mode-hook (quote ((lambda nil (let* ((name "<return>") (key (read-kbd-macro name)) (binding (lookup-key (or html-mode-map global-map) key))) (let ((entry (assoc (cons name (quote html-mode-map)) personal-keybindings))) (if entry (setq personal-keybindings (delq entry personal-keybindings)))) (setq personal-keybindings (cons (list (cons name (quote html-mode-map)) (quote newline-and-indent) (if (numberp binding) nil binding)) personal-keybindings)) (define-key (or html-mode-map global-map) key (quote newline-and-indent)))) zencoding-mode)) t)
 '(httpd-root "~")
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 16 -1) " " (size 6 -1 :right) " " (mode 16 16) " " filename) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-maybe-show-regexps nil)
 '(ibuffer-saved-filter-groups (quote (("default" ("Commands" (or (mode . shell-mode) (mode . eshell-mode) (mode . term-mode) (mode . compilation-mode))) ("Helm" (mode . helm-mode)) ("Magit" (or (mode . magit-status-mode) (mode . magit-log-mode))) ("C++" (or (mode . c-mode) (mode . c++-mode))) ("Lisp" (mode . emacs-lisp-mode)) ("Dired" (mode . dired-mode)) ("Gnus" (or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode) (name . "^\\.newsrc-dribble"))) ("Org" (or (name . "^\\*Calendar\\*$") (name . "^diary$") (mode . org-mode))) ("Emacs" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-shrink-to-minimum-size t t)
 '(ibuffer-use-other-window t)
 '(ido-auto-merge-delay-time 3.0)
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-enable-tramp-completion nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-everywhere t)
 '(ido-file-extensions-order (quote (".org")))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.DS_Store" "\\`\\.localized" "\\.sparsebundle/" "\\.dmg\\'")))
 '(ido-max-directory-size 100000)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/data/ido.last")
 '(ido-use-virtual-buffers t)
 '(image-dired-dir "~/.emacs.d/data/image-dired/")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/")
 '(initial-scratch-message nil)
 '(initsplit-customizations-alist (quote (("\\`\\(gnus\\|nn\\|message\\|mail\\|mm-\\|smtp\\|send-mail\\|check-mail\\|spam\\|sc-\\)" "~/.emacs.d/gnus-settings.el" nil nil) ("\\`\\(org-\\)" "~/.emacs.d/org-settings.el" nil nil))))
 '(js2-global-externs (quote ("$" "jQuery")))
 '(keyfreq-file "~/.emacs.d/data/.emacs.keyfreq")
 '(kill-whole-line t)
 '(line-number-mode t)
 '(ns-function-modifier (quote hyper))
 '(nxhtml-menu-mode t)
 '(org-M-RET-may-split-line (quote ((headline . t) (item . t) (default . t))))
 '(org-adapt-indentation nil)
 '(org-agenda-compact-blocks nil)
 '(org-agenda-deadline-leaders (quote ("Deadline:  " "In %3d d.: " "%2d d. ago: ")))
 '(org-agenda-diary-file (quote diary-file))
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-export-html-style "<link rel=\\\"stylesheet\\\" href=\\\"http://scarcecapital.com/style/css/scarce.css\\\" type=\\\"text/css\\\" />")
 '(org-agenda-files (quote ("~/git/hq/other/hqdm.org" "~/stuff/strategy/strategy.org" "~/stuff/hq/hq.org" "~/stuff/emacs/emacs.org" "~/stuff/content/super.org" "~/stuff/opoker/opoker.org" "~/stuff/content/life.org" "~/git/newsite/newsite.org" "~/stuff/haskell/haskell-notes.org" "~/stuff/algo/algo.org" "~/stuff/factor/rates.org" "~/stuff/factor/factor.org" "~/stuff/factor/equities.org" "~/git/hyperqr/hyperqr.org" "~/git/hyperq/hyperq.org" "~/stuff/sys/webdev.org" "~/stuff/org/org.org" "~/stuff/quant/garch.org" "~/git/o-blog/o-blog.org" "~/.emacs.d/README.org" "~/.emacs.d/dotemacs.org" "~/stuff/stuff.org" "~/stuff/site/allocation_preso.org" "~/stuff/site/production.org" "~/stuff/content/writing.org" "~/stuff/content/tail-material.org" "~/stuff/emacs/bindings.org" "~/stuff/org/refile.org" "~/stuff/org/bugz.org" "~/stuff/quant/rebal.org" "~/stuff/quant/volatility.org" "~/stuff/quant/strategy_run_annotated.org" "~/stuff/quant/strategy_run_default.org" "~/stuff/quant/momentum.org" "~/stuff/quant/mom-report.org" "~/stuff/quant/jgb.org" "~/stuff/quant/data.org" "~/stuff/quant/deadzone.org" "~/stuff/quant/moments.org" "~/stuff/quant/da.org" "~/stuff/sys/sys.org")))
 '(org-agenda-include-diary nil)
 '(org-agenda-ndays 1)
 '(org-agenda-persistent-filter t)
 '(org-agenda-scheduled-leaders (quote ("" "S%d: ")))
 '(org-agenda-skip-additional-timestamps-same-entry t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy (quote ((agenda habit-down time-up todo-state-up user-defined-up priority-down effort-up category-keep) (todo category-up priority-down effort-up) (tags category-up priority-down effort-up) (search category-keep))))
 '(org-agenda-span (quote day))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-tags-column -102)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-agenda-use-time-grid nil)
 '(org-agenda-window-setup (quote current-window))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (sh . t) (dot . t) (org . t) (js . t) (R . t) (haskell \.t))))
 '(org-babel-post-tangle-hook (quote ((lambda nil (progn (delete-leading-whitespace) (save-buffer))))))
 '(org-babel-results-keyword "results")
 '(org-capture-templates (quote (("," ",," entry (file "~/stuff/org/refile.org") "* %?
") ("t" "todo") ("tu" "urgent todo" entry (file "~/stuff/org/refile.org") "* NEXT %? :urgent:
:PROPERTIES:
:OPEN: %U
:END:
") ("tl" "linked todo" entry (file "~/stuff/org/refile.org") "* TODO %?
%a
") ("tn" "next todo" entry (file "~/stuff/org/refile.org") "* NEXT %?
") ("tt" "todo todo" entry (file "~/stuff/org/refile.org") "* TODO %?
") ("tb" "yank body" entry (file "~/stuff/org/refile.org") "* TODO %?
%c
") ("th" "yank header" entry (file "~/stuff/org/refile.org") "* TODO %c
%?
") ("z" "bugz" entry (file+headline "~/stuff/org/bugz.org" "incoming") "* TODO %?
%a") ("s" "snipz") ("sr" "snipz request" entry (file+headline "~/stuff/org/snipz.org" "incoming") "* TODO %?
%a
") ("sn" "snipz note" entry (file+headline "~/stuff/org/snipz.org" "incoming") "* %?
%c
") ("b" "binding" table-line (file+headline "~/stuff/emacs/bindings.org" "incoming")) ("k" "kill ring") ("kb" "kill ring body" entry (file "~/stuff/org/refile.org") "* %?
%c
") ("kh" "kill ring head" entry (file "~/stuff/org/refile.org") "* %c
%?
"))))
 '(org-clock-clocked-in-display nil)
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-confirm-elisp-link-function nil)
 '(org-crypt-disable-auto-save nil)
 '(org-crypt-key "F0B66B40")
 '(org-cycle-global-at-bob t)
 '(org-cycle-separator-lines 0)
 '(org-deadline-warning-days 5)
 '(org-default-notes-file "~/stuff/org/refile.org")
 '(org-directory "~/stuff/org")
 '(org-ditaa-jar-path "~/java/ditaa0_6b.jar")
 '(org-edit-src-content-indentation 0)
 '(org-enable-priority-commands nil)
 '(org-enforce-todo-dependencies t)
 '(org-entities-user (quote (("hardbreak" "\\\\" nil "" "" "" "") ("greytext" "\\\\color{greytext}" nil "" "" "" ""))))
 '(org-export-babel-evaluate nil)
 '(org-export-copy-to-kill-ring t)
 '(org-export-date-timestamp-format "%Y-%m-%d")
 '(org-export-html-inline-images t)
 '(org-export-html-link-home "www.scarcecapital.com")
 '(org-export-html-postamble nil)
 '(org-export-html-style "")
 '(org-export-html-style-extra "")
 '(org-export-html-style-include-default nil)
 '(org-export-html-xml-declaration (quote (("html" . "") ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>") ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))
 '(org-export-with-drawers nil)
 '(org-export-with-tags nil)
 '(org-export-with-timestamps nil)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-footnote-section nil)
 '(org-from-is-user-regexp "\\<tony day\\>")
 '(org-hide-leading-stars nil)
 '(org-html-head-include-default-style nil)
 '(org-html-head-include-scripts nil)
 '(org-html-htmlize-output-type (quote css))
 '(org-html-postamble nil)
 '(org-insert-heading-respect-content nil)
 '(org-latex-to-pdf-process (quote ("xelatex -interaction nonstopmode -output-directory %o %f" "xetex %f")))
 '(org-link-abbrev-alist (quote (("google" . "http://www.google.com/search?q="))))
 '(org-log-done nil)
 '(org-log-into-drawer "LOGBOOK")
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-inbox-for-pull "~/stuff/org/refile.org")
 '(org-modules (quote (org-gnus org-id org-info org-checklist)))
 '(org-outline-path-complete-in-steps nil)
 '(org-plantuml-jar-path "~/java/plantuml.jar")
 '(org-provide-todo-statistics nil)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-target-verify-function nil)
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))
 '(org-refile-use-outline-path (quote file))
 '(org-remove-highlights-with-change nil)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-src-lang-modes (quote (("plantuml" . fundamental) ("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist) ("asymptote" . asy) ("dot" . graphviz-dot) ("sqlite" . sql) ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("screen" . shell-script) ("R" . R) ("haskell" . haskell))))
 '(org-src-preserve-indentation t)
 '(org-startup-folded (quote content))
 '(org-startup-indented t)
 '(org-support-shift-select (quote always))
 '(org-taskjuggler-default-global-properties "shift s40 \"Part time shift\" {
  workinghours wed, thu, fri off
}

account costs \"costs\" {
	account salary \"salary\" {
		account strat \"Strategy costs\" { aggregate resources }
		account quant \"Quant costs\"  { aggregate resources }
		account dev \"IT development\"  { aggregate resources }
		account trade \"Trading costs\"  { aggregate resources }
		account admin \"Office management\"  { aggregate resources }
		account bdg \"Blue Diamond Global costs\"  { aggregate resources }
		}
	account nonsalary \"capital and income\" {
		account office \"Office costs\"
		account hard \"Hardware costs\"
		account soft \"Software costs\"
		account travel \"Travel expenses\"
		account licence \"Licencing costs\"
		account services \"corporate services\"
	}
	account main \"maintenance\"
}

account fees \"management fees\"
balance costs fees

navigator navbar {
  hidereport @none
}

macro TaskTip [
  tooltip istask() -8<-
    '''Start: ''' <-query attribute='start'->
    '''End: ''' <-query attribute='end'->
    ----
    '''Resources:'''

    <-query attribute='resources'->
    ----
    '''Precursors: '''

    <-query attribute='precursors'->
    ----
    '''Followers: '''

    <-query attribute='followers'->
    ->8-
]

flags external

")
 '(org-taskjuggler-default-project-duration 350)
 '(org-taskjuggler-default-reports (quote ("
textreport frame \"\" {
  header -8<-
    == BD Australia Project ==
    <[navigator id=\"navbar\"]>
  ->8-
  footer \"----\"
  textreport index \"overview\" {
    formats html
    center '<[report id=\"overview\"]>'
  }
taskreport \"tasks\" {
  headline \"task report\"
  columns name, start, end, duration, effort, chart
  loadunit days
  sorttasks tree
  formats html
}
resourcereport \"resources\" {
  headline \"resource usage\"
  columns bsi, name, effort {width 60 halign @ all right}, cost {width 100 halign @ all right}, chart
  loadunit days
  sortresources tree
  formats html
}

resourcereport \"stafftask\" {
  headline \"staff by task allocation\"
  columns name, effort {width 100 halign @ all right}, chart
  loadunit days
  hidetask treelevel()>3
  hideresource external
  sortresources tree
  formats html
}

resourcereport \"BDG\" {
  headline \"Blue Diamond Global support\"
  columns name, effort {width 100 halign @ all right}, chart
  loadunit days
  resourceroot bdg
  hidetask @ none
  sortresources tree
  formats html
}


accountreport \"profitloss\" {
  headline \"P&L\"
  columns name, monthly
  formats html
}


}

taskreport overview \"\" {
  header -8<-
    === Project Overview ===

    The project is structured into 4 phases.

    # Office creation
    # Investment process establishment
    # Testing
    # Seeding and production

    === Original Project Plan ===
  ->8-
  columns name, start, duration, effort {title \"salary days\"}, cost {title \"non-salary costs\"},
	  chart { ${TaskTip} }
  timeformat \"%Y-%m-%d\"
  loadunit days
  hideresource @all
  hidetask treelevel()>2
  balance costs fees
  sorttasks tree


  footer -8<-
    === Current Status ===

  ->8-
}
")))
 '(org-taskjuggler-keep-project-as-task nil)
 '(org-taskjuggler-valid-resource-attributes (quote (limits vacation shift booking efficiency journalentry rate workinghours flags chargeset)))
 '(org-taskjuggler-valid-task-attributes (quote (account start note duration endbuffer endcredit end flags journalentry length limits maxend maxstart minend minstart period reference responsible scheduling startbuffer startcredit statusnote chargeset charge priority numberformat currencyformat)))
 '(org-todo-keyword-faces (quote (("TODO" :foreground "#6775ac" :weight bold) ("NEXT" :foreground "#bc8878" :weight bold) ("DONE" :foreground "forest green" :weight bold) ("WAITING" :foreground "orange" :weight bold) ("HOLD" :foreground "magenta" :weight bold) ("CANCELLED" :foreground "forest green" :weight bold) ("PHONE" :foreground "forest green" :weight bold))))
 '(org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts nil)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("ELPA" . "http://tromey.com/elpa/") ("Marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-enable-at-startup nil)
 '(persistent-scratch-file-name "~/.emacs.d/data/scratch")
 '(read-buffer-function (quote ido-read-buffer))
 '(reb-re-syntax (quote string))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(rng-nxml-auto-validate-flag nil)
 '(rng-schema-locating-files (quote ("~/.emacs.d/site-lisp/nxhtml/etc/schemas/html5-schemas.xml" "schemas.xml" "/Applications/Emacs241012.app/Contents/Resources/etc/schema/schemas.xml" "/Users/tonyday/.emacs.d/rnc/html5/html5-schemas.xml")))
 '(same-window-buffer-names (quote ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-kill-file-name "~/.emacs.d/data/kill-ring-saved.el")
 '(save-place-file "/Users/tonyday/.emacs.d/places")
 '(scroll-bar-mode nil)
 '(semantic-default-submodes (quote (global-semantic-highlight-func-mode global-semantic-decoration-mode global-semantic-stickyfunc-mode global-semantic-idle-completions-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode global-cedet-m3-minor-mode)))
 '(sentence-end-double-space nil)
 '(shift-select-mode t)
 '(slime-kill-without-query-p t)
 '(slime-repl-history-file "~/.emacs.d/data/slime-history.eld")
 '(slime-startup-animation nil)
 '(smex-save-file "~/.emacs.d/data/.smex-items")
 '(tool-bar-mode nil)
 '(tramp-backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(tramp-default-host "localhost")
 '(tramp-default-method "ssh")
 '(tramp-default-method-alist (quote ((nil "%" "smb") ("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost\\|silver\\)\\'" "\\`root\\'" "su") (nil "\\`\\(anonymous\\|ftp\\)\\'" "ftp") ("\\`ftp\\." nil "ftp"))))
 '(tramp-default-proxies-alist (quote (("\\`localhost\\'" nil nil) ("\\`127.0.0.1\\'" nil nil) ("\\`.+\\'" "\\`root\\'" "/ssh:%h:"))) nil nil "
Gets around the common setting that prohibits ssh login as root.

Don't do any proxying for connections to localhost (depends
on the customization of tramp-default-host to \"localhost\" for simple
matching), and otherwise, if sudo'ing somewhere, ssh there first and
then sudo on the remote host itself.")
 '(tramp-persistency-file-name "~/.emacs.d/data/tramp")
 '(tramp-remote-path (quote (tramp-default-remote-path "/usr/sbin" "/usr/local/sbin" "/usr/local/bin" "/sbin" "/local/bin")))
 '(tramp-verbose 3)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "Tony Day")
 '(user-mail-address "tonyday567@gmail.com")
 '(validator-script "~/.emacs.d/site-lisp/flymake-html-validator")
 '(vc-diff-switches "-du")
 '(visible-bell t)
 '(visible-cursor t)
 '(w3m-command "/usr/local/bin/w3m" t)
 '(w3m-confirm-leaving-secure-page t nil nil "
I never like being nannied by regular browsers either.")
 '(w3m-cookie-accept-domains (quote ("www.google.com")))
 '(w3m-default-display-inline-images t)
 '(w3m-display-ins-del nil)
 '(w3m-fill-column -50 nil nil "
When I use variable-pitch-mode the text tends to run off the right 
side of the window.  This drastic setting was what I needed to prevent that.")
 '(w3m-use-cookies t)
 '(whitespace-auto-cleanup t)
 '(whitespace-line-column 80)
 '(whitespace-rescan-timer-time nil)
 '(whitespace-silent t)
 '(whitespace-style (quote (face trailing space-after-tab::space space-before-tab::space indentation::space empty)))
 '(yas-fallback-behavior (quote call-other-command))
 '(yas-prompt-functions (quote (yas-ido-prompt yas-dropdown-prompt)))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets")) nil (yasnippet))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t)
 '(yas/triggers-in-field t)
 '(yas/wrap-around-region t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco"))))
 '(cursor ((t (:foreground "#3f3f3f" :background "#7fff00"))))
 '(diff-added ((((background dark)) (:foreground "#FFFF9B9BFFFF")) (t (:foreground "DarkGreen"))) t)
 '(diff-changed ((((background dark)) (:foreground "Yellow")) (t (:foreground "MediumBlue"))) t)
 '(diff-context ((((background dark)) (:foreground "White")) (t (:foreground "Black"))) t)
 '(diff-file-header ((t (:background "Black" :foreground "dark gray" :weight bold))) t)
 '(diff-header ((((background dark)) (:foreground "Cyan")) (t (:foreground "Red"))) t)
 '(diff-index ((((background dark)) (:foreground "Magenta")) (t (:foreground "Green"))) t)
 '(diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474")) (t (:foreground "DarkBlue"))) t)
 '(ess-function-call-face ((t (:foreground "#7a92d4" :slant normal))) t)
 '(ess-numbers-face ((t (:foreground "#7fa08f" :slant normal))) t)
 '(flymake-errline ((t (:foreground "light green" :underline t :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#bf89d1"))))
 '(font-lock-comment-face ((t (:foreground "#93A1A1"))))
 '(font-lock-constant-face ((t (:foreground "#b9a16c"))))
 '(font-lock-function-name-face ((t (:foreground "#268bd2"))))
 '(font-lock-keyword-face ((t (:foreground "#859900"))))
 '(font-lock-string-face ((t (:foreground "#2aa198"))))
 '(font-lock-type-face ((t (:inherit default :foreground "#b58900" :weight normal))))
 '(font-lock-variable-name-face ((t (:foreground "#657b83"))))
 '(helm-M-x-key ((t (:foreground "#7f9f7f" :underline t))))
 '(helm-candidate-number ((t (:background "#faffb5" :foreground "black"))))
 '(helm-selection ((t (:background "alternateSelectedControlColor" :underline t))))
 '(helm-separator ((t (:foreground "#ffbfb5"))))
 '(helm-source-header ((t (:background "#abd7f0" :foreground "black" :underline t))))
 '(helm-visible-mark ((t (:background "#d1f5ae"))))
 '(hl-line ((t (:inherit (zenburn-highlight-damp)))) t)
 '(magit-branch-face ((((class color) (background light)) (:foreground "Blue"))) t)
 '(magit-diff-none-face ((((class color) (background light)) (:foreground "grey50"))) t)
 '(magit-header ((t (:weight bold))) t)
 '(magit-item-highlight ((t nil)) t)
 '(magit-topgit-current ((t nil)) t)
 '(match ((t (:background "disabledControlTextColor" :foreground "controlHighlightColor" :weight bold))))
 '(org-column ((t (:background "#505050" :strike-through nil :underline nil :slant normal :weight normal :height 120 :family "Monaco"))))
 '(org-mode-line-clock ((t (:foreground "grey50" :box (:line-width -1 :style released-button)))) t)
 '(org-scheduled-previously ((t (:foreground "#dcdccc"))))
 '(trailing-whitespace ((t (:background "disabledControlTextColor"))))
 '(whitespace-empty ((t (:background "keyboardFocusIndicatorColor" :foreground "#cc9393"))))
 '(whitespace-indentation ((t (:background "#3f3f3f" :foreground "#cc9393"))))
 '(whitespace-line ((t (:background "#3f3f3f"))))
 '(whitespace-trailing ((t (:background "#859900")))))
