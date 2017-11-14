;;; package management

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

;;; markdown

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;; user experience

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

;;; modeline
;; diminish lighters
(use-package diminish)
;; file size
(size-indication-mode)
;; column number
(column-number-mode t)
;; nice modile
(use-package smart-mode-line)

;;; columns
(defconst help/column-width 78)
(setq-default fill-column help/column-width)
(setq colon-double-space nil)

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

;;;; pie keymap

;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

(defvar pie-mode-map (make-sparse-keymap)
  "Keymap for `pie-mode'.")

;; can quit
(define-key pie-mode-map (kbd "C-M-Q") #'save-buffers-kill-emacs)

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
(define-key pie-mode-map (kbd "C-M-j") #'beginning-of-line)
(define-key pie-mode-map (kbd "C-M-l") #'end-of-line)

;; typical buffer
(define-key pie-mode-map (kbd "C-f") #'switch-to-buffer)
(define-key pie-mode-map (kbd "M-f") #'find-file)

;; typical command
(define-key pie-mode-map (kbd "C-g") #'execute-extended-command)

;; utilities
(define-key pie-mode-map (kbd "M-z") #'undo-tree-visualize)

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



