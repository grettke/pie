;; only stable packages
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; for every package use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; no Emacs startup screen
(setq inhibit-startup-screen t)

;; no scratch
(kill-buffer "*scratch*")

;; default major mode Markdown
(setq initial-major-mode 'markdown-mode)

;; always start to an empty buffer
(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "Untitled.md")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))
(setq initial-buffer-choice 'xah-new-empty-buffer)

;; fast answers
(fset 'yes-or-no-p 'y-or-n-p)

;; echo keystrokes immediately.
(setq echo-keystrokes 0.02)

;; modeline
;; diminish lighters
(use-package diminish)
;; file size
(size-indication-mode)
;; column number
(column-number-mode t)
;; nice modile
(use-package smart-mode-line)

;; columns
(defconst help/column-width 78)
(setq-default fill-column help/column-width)
(setq colon-double-space nil)
(use-package fill-column-indicator
  :config
  (setq fci-rule-column 79))

;; buffers
(desktop-save-mode t)
(setq desktop-restore-eager 10)

;; sane undo
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode)

;; files end with newlines
(setq require-final-newline t)

;; what happens when scrolling
(setq track-eol t)
(setq line-move-visual nil)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 101)

;; file based system
(setq auto-save-default t)
(setq make-backup-files nil)
(setq auto-save-visited-file-name t)
(setq auto-save-interval 0)
(setq auto-save-timeout (* 60 5))
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)
(defun help/create-non-existent-directory ()
  "Attribution URL: `https://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/'"
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist. Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'help/create-non-existent-directory)
(setq large-file-warning-threshold (* 1024 1024 2))
(setq temporary-file-directory "/tmp")
(use-package hardhat
  :ensure t
  :diminish global-hardhat-mode
  :config
  (global-hardhat-mode 1))

;; menuing
(use-package imenu
  :config
  (setq imenu-sort-function #'imenu--sort-by-name))
(defun help/try-to-add-imenu ()
  "Add Imenu to modes that have `font-lock-mode' activated.

Attribution: URL http://www.emacswiki.org/emacs/ImenuMode"
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook #'help/try-to-add-imenu)
(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-position 'left)
  (setq imenu-list-size 40))

;; pie keymap

;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

(defvar pie-mode-map (make-sparse-keymap)
  "Keymap for `pie-mode'.")

;; can quit
(define-key pie-mode-map (kbd "C-M-Q") #'save-buffers-kill-emacs)

(use-package mwim)

;; typical navigation
(define-key pie-mode-map (kbd "C-i") #'previous-line)
(define-key pie-mode-map (kbd "C-k") #'next-line)
(define-key pie-mode-map (kbd "C-j") #'backward-char)
(define-key pie-mode-map (kbd "C-l") #'forward-char)
(define-key pie-mode-map (kbd "M-i") #'scroll-down-command)
(define-key pie-mode-map (kbd "M-k") #'scroll-up-command)
(define-key pie-mode-map (kbd "M-j") #'backward-word)
(define-key pie-mode-map (kbd "M-l") #'forward-word)
(define-key pie-mode-map (kbd "C-M-i") #'beginning-of-buffer)
(define-key pie-mode-map (kbd "C-M-k") #'end-of-buffer)
(define-key pie-mode-map (kbd "C-M-j") #'mwim-beginning-of-code-or-line)
(define-key pie-mode-map (kbd "C-M-l") #'end-of-line)

(advice-add #'backward-page :after #'recenter)
(advice-add #'forward-page :after #'recenter)

;; special navigation
(use-package avy)
(define-key pie-mode-map (kbd "C-p") #'avy-goto-word-1)
(define-key pie-mode-map (kbd "M-p") #'avy-goto-char)
(define-key pie-mode-map (kbd "C-M-p") #'avy-pop-mark)

;; typical buffer
(define-key pie-mode-map (kbd "C-f") #'switch-to-buffer)
(define-key pie-mode-map (kbd "M-f") #'find-file)
(define-key pie-mode-map (kbd "C-M-f") #'ibuffer)

;; typical command
(define-key pie-mode-map (kbd "C-g") #'execute-extended-command)

;; utilities
(define-key pie-mode-map (kbd "M-z") #'undo-tree-visualize)
(define-key pie-mode-map (kbd "C-M-M") #'imenu-list)

;; interactively do things
(use-package ido)
(use-package flx-ido
  :ensure t
  :config
  (ido-mode t))
(use-package ido-hacks
  :ensure t)
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode t)
  (setq ido-create-new-buffer 'always)
  (flx-ido-mode t)
  (setq ido-use-faces nil))
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
(use-package smex
  :ensure t
  :config
  (smex-initialize))
(setq ido-use-url-at-point t)
(setq ido-use-filename-at-point 'guess)

;;;###autoload
(define-minor-mode pie-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-pie-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter ""
  :keymap pie-mode-map)

;;;###autoload
(define-globalized-minor-mode global-pie-mode pie-mode pie-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((pie-mode . ,pie-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-pie-mode ()
  "Turn off pie-mode."
  (pie-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-pie-mode)

(pie-mode)
;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/



