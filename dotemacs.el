
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/emacs/packages")



(require 'use-package)
(require 'bind-key)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-z") 'suspend-frame)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-S-d") 'duplicate-line)

(use-package autopair
             :init
             (progn
              (autopair-global-mode 1)))

(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [up]
       'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [down]
       'comint-next-matching-input-from-input)
     ;; also recommended for ESS use --
     (setq comint-scroll-to-bottom-on-output -1)
     (setq comint-scroll-show-maximum-output -1)
     ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt
     (setq comint-scroll-to-bottom-on-input 1)
     ))
(setq ess-tab-complete-in-script t)
(setq-default ess-dialect "R")
(use-package ess-site
  :mode ("\\.[rR]\\'" . R-mode)
  :bind ("C-. C-. r" . R))

(use-package helm-config
  :init
  (progn
    (bind-key "M-x" 'helm-M-x)
    (bind-key "C-h a" 'helm-apropos)
    (bind-key "M-s a" 'helm-do-grep)
    (bind-key "M-s b" 'helm-occur)
    (bind-key "M-s F" 'helm-for-files)
    (bind-key "C-x f" 'helm-find-git-file)
    (bind-key "C-h 2" 'helm-info-org)
    (bind-key "C-h 3" 'helm-info-emacs)
    (bind-key "C-h 4" 'helm-info-elisp)
    (bind-key "C-h 5" 'helm-locate-library)
    (bind-key "C-h 6" 'helm-locate))
  :config
  (helm-match-plugin-mode t))

(use-package ido
  :init
  (ido-mode t)
)

(require 'minimal)
(minimal-mode 1)
(nconc default-frame-alist '((cursor-type . bar)))
(setq inhibit-startup-message t)

(require 'move-text)
(move-text-default-bindings)

(use-package multiple-cursors
:init
(progn
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
)

(add-to-list 'load-path "~/emacs/packages/expand-region")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package paren
:init
(show-paren-mode 1)
)

(use-package winner
:init
(progn
(winner-mode 1)
(global-set-key (kbd "M-j")  'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-i")    'windmove-up)
(global-set-key (kbd "M-k")  'windmove-down)
)
)
