;;; package --- Some useful routines based on flycheck errors in haskell mode

;;; Commentary:

;;; Code:

(defun haskell-flycheck-insert-lints ()
  "Insert flycheck hint at point."
  (interactive)
  (let ((errs (flycheck-overlay-errors-at (point))))
    (when errs
      (-each errs 'haskell-flycheck-insert-lint))))

(defun haskell-flycheck-insert-type-bindings ()
  "Insert all flycheck type binding guesses."
  (interactive)
  (goto-char (point-min))
  (message
        "added %d type sigs"
        (fromMaybe 0 (let*
             ((added 0)
              (changes
               (mapcar
                (lambda (x)
                  (when
                      (haskell-flycheck-is-type-bind x)
                    (goto-line
                     (+ added
                        (flycheck-error-line x)))
                    (haskell-flycheck-insert-sig-at-point x)
                    (setq added (+ 1 added))))
                flycheck-current-errors)))
           (car (last (delq nil changes)))))))

(defun haskell-flycheck-insert-type-binding ()
  "Insert the flycheck guess at the type binding."
  (interactive)
  (haskell-flycheck-insert-sig (flycheck-overlay-errors-at (point))))

(defun haskell-flycheck-insert-sig-at-point (err)
  (let ((msg
         (haskell-flycheck-get-sig (flycheck-error-message err))))
    (when msg
      (insert (haskell-flycheck-clean-sig msg)))))

(defun haskell-flycheck-insert-sig (errs)
  (when errs
    (-each errs
           (lambda (e)
             (let ((msg
                    (haskell-flycheck-get-sig (flycheck-error-message e))))
               (when msg
                 (insert (haskell-flycheck-clean-sig msg))))))))

(defun haskell-flycheck-is-type-bind (e)
  (string-match
   (haskell-flycheck-typemessage-re)
   (flycheck-error-message e)))

(defun haskell-flycheck-get-sig (msg)
  (when (string-match
         (haskell-flycheck-typemessage-re)
         msg)
    (match-string 1 msg)))

(defun haskell-flycheck-clean-sig (msg)
  (with-temp-buffer
  (insert msg)
  (goto-char (point-min))
  (replace-regexp
   (haskell-flycheck-forall-re)
   "")
  (goto-char (point-min))
  (replace-regexp (rx "=>") "=> ")
  (goto-char (point-min))
  (replace-regexp
   (rx
    (and
     (1+ "\n")
     (0+ blank)))
   "")
  (goto-char (point-max))
  (insert "\n")
  (buffer-string)))

(defun haskell-flycheck-forall-re ()
  (rx
   (and
    "forall"
    (1+ any)
    (1+ ".")
    (0+ blank))))

(defun haskell-flycheck-typemessage-re ()
  (rx
   (and
    "Top-level binding with no type signature:"
    (0+ "\n")
    (0+ blank)
    (group
     (0+ (or
          any
          "\n"))))))


(defun haskell-flycheck-hlint-re ()
  (rx
   (and
    "Found:\n"
    (0+ blank)
    (group (1+ (or any "\n")))
    (1+ "\n")
    "Why not:\n"
    (0+ blank)
    (group (1+ (or any "\n"))))))

(defun haskell-flycheck-hlint-line-re ()
  (rx
   (and
    (0+ blank)
    (group (1+ any)))))

(defun haskell-flycheck-lint-lines (str)
  (with-temp-buffer
    (goto-char (point-min))
    (delete-to-end-of-buffer)
    (insert str)
    (goto-char (point-min))
    (let ((parts nil))
      (while
          (search-forward-regexp (haskell-flycheck-hlint-line-re) (point-max) t)
        (push (match-string 1) parts))
      (reverse parts))))

(defun haskell-flycheck-hlint-parts (hlint-msg)
  (with-temp-buffer
    (insert hlint-msg)
    (goto-char (point-min))
    (search-forward-regexp (haskell-flycheck-hlint-re) (point-max) t)
    ;; get individual lines
    (let* ((sfound (match-string 1))
           (swhynot (match-string 2)))
      (list
       (haskell-flycheck-lint-lines sfound)
       (haskell-flycheck-lint-lines swhynot)))))

(defun haskell-flycheck-main-re (parts)
  (mapconcat
   'identity
   (mapcar
    (lambda (x) (regexp-opt (list x)))
    (car parts))
   "\\(\\(?:[[:blank:]]\\|\n\\)+\\)"))

(defun haskell-flycheck-fix-lambda (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (if (search-forward-regexp "\\\\ " (point-max) t)
        (progn
          (replace-match "\\\\")
            (buffer-string))
      str)))

(defun haskell-flycheck-insert-lint (err)
  (save-excursion
    (let* ((msg (flycheck-error-message err))
           (spot (flycheck-error-line err))
           (parts
            (mapcar (lambda (x) (mapcar 'haskell-flycheck-fix-lambda x)) (haskell-flycheck-hlint-parts msg)))
           (re (haskell-flycheck-main-re parts)))
      (when (search-forward-regexp re (point-max) t)
        (replace-match
         (reduce
          #'concat
          (haskell-flycheck-interleave
           (nth 1 parts)
           (mapcar
            'match-string
            (number-sequence 1 (- (length (nth 1 parts)) 1))))))))))

(defun haskell-flycheck-interleave (l1 l2)
"(haskell-flycheck-interleave (list 1 1 1) (list 0 0))  '(1 0 1 0 1)."
  (cond ((and (eql l1 nil) (eql l2 nil)) nil)         ;; rule #1
        ((eql l1 nil) (cons nil (haskell-flycheck-interleave l2 l1)))  ;; rule #2, current value is nil
        (t (cons (first l1) (haskell-flycheck-interleave l2 (cdr l1)))))) ;; rule #3 in all
;; other cases


(flycheck-define-checker haskell-ghc
  "A Haskell syntax and type checker using ghc.

See URL `http://www.haskell.org/ghc/'."
  :command ("ghc" "-Wall" "-fno-code" "-isrc:./:../src:../../src:../../../src:dev:../dev:../../dev:test:../test:../../test" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (one-or-more " ")
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n" (one-or-more " ")
                   (message (one-or-more not-newline)
                            (zero-or-more "\n"
                                          (one-or-more " ")
                                          (one-or-more not-newline)))))
          line-end))
  :modes haskell-mode
  :next-checkers ((warnings-only . haskell-hlint)))


    (flycheck-define-checker haskell-only-ghc
      "A Haskell syntax and type checker using ghc.

See URL `http://www.haskell.org/ghc/'."
      :command ("ghc" "-Wall" "-fno-code" "-isrc:./:../src:../../src:../../../src:dev:../dev:../../dev:test:../test:../../test" source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ":"
                (or " " "\n    ") "Warning:" (optional "\n")
                (one-or-more " ")
                (message (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (one-or-more " ")
                                       (one-or-more not-newline)))
                line-end)
       (error line-start (file-name) ":" line ":" column ":"
              (or (message (one-or-more not-newline))
                  (and "\n" (one-or-more " ")
                       (message (one-or-more not-newline)
                                (zero-or-more "\n"
                                              (one-or-more " ")
                                              (one-or-more not-newline)))))
              line-end))
      :modes haskell-mode)

    (flycheck-define-checker haskell-cabal
      "A Haskell syntax and type checker based on cabal repl.

      See URL `http://www.haskell.org/ghc/'."
      :command ("ghc" "-Wall" "-fno-code" "-package-db" ".cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d" source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ":"
                (or " " "\n    ") "Warning:" (optional "\n")
                (one-or-more " ")
                (message (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (one-or-more " ")
                                       (one-or-more not-newline)))
                line-end)
       (error line-start (file-name) ":" line ":" column ":"
              (or (message (one-or-more not-newline))
                  (and "\n" (one-or-more " ")
                       (message (one-or-more not-newline)
                                (zero-or-more "\n"
                                              (one-or-more " ")
                                              (one-or-more not-newline)))))
              line-end))
      :modes haskell-mode)



(defun fromMaybe (default x)
  (if x
      x
    default))


(provide 'haskell-flycheck)
;;; haskell-flycheck ends here
