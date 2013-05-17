;;; init.el --- Derived from Milkmacs configuration file
;;; http://milkbox.net/note/single-file-master-emacs-configuration/

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(setenv "EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

(setq require-final-newline t)
(setq default-mode-line-format
      '("" "%[%f %*+%* %l (" mode-name minor-mode-alist ")%]" mode-line-process " " mode-line-buffer-identification "%-"))

;;;; Commands that trip me up - or not 

(put 'downcase-region 'disabled t)
(put 'upcase-region 'disabled t)
(put 'narrow-to-region 'disabled nil)

(put 'eval-expression 'disabled nil)

(setq-default tab-width 4)



(require 'cl)
(require 'misc)
(require 'midnight)
(require 'saveplace) (setq-default save-place t)
(require 'uniquify) (progn
		      uniquify-buffer-name-style 'forward
		      uniquify-ask-about-buffer-names-p t
		      uniquify-ignore-buffers-re "^\\*")
(require 'checkdoc)

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defvar bem-packages 
  '(exec-path-from-shell
    browse-kill-ring
    ido-ubiquitous
    ido-vertical-mode
    clojure-mode
    ace-jump-mode
    ag
    browse-kill-ring
    expand-region
    gist
    iy-go-to-char
    hl-sexp
    markdown-mode+
    rainbow-delimiters
    smex
    ))


(defun bem-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc '(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
	bem-packages))



;;;; Paths

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(ignore-errors (server-start))

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;;; global key bindings

(global-set-key (kbd "C-x f") 'find-file-in-project-other-window)
(global-set-key "\e%" 'query-replace-regexp)


(global-set-key (kbd "s-j") 'windmove-right)
(global-set-key (kbd "s-k") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)

(global-set-key [f1] 'recentf-open-files)

(global-set-key (kbd "C-h l") 'ace-jump-line-mode)
(global-set-key (kbd "C-h C-d") 'ace-jump-word-mode)

(global-set-key (kbd "s-P") 'ag-project)
(global-set-key (kbd "s-p") 'ag-project-at-point)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "M-z") 'execute-extended-command)

(global-set-key (kbd "s-w") 'er/expand-region)

(global-set-key (kbd "C-c C-f") 'iy-go-to-char)
(global-set-key (kbd "C-c C-b") 'iy-go-to-char-backward)

(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key [f8] 'hippie-expand)

(global-set-key "\e!" 'line-to-top-of-window)

;;;; global settings

(show-paren-mode t)
(delete-selection-mode t)
(global-auto-revert-mode t)
(browse-kill-ring-default-keybindings)  ; M-y - n for next, q to quit

;;;; recentf
(recentf-mode t)
(setq-default recentf-max-menu-items 40)
(setq-default recentf-menu-filter 'recentf-sort-basenames-ascending)


;;;; Darwin

(cond ((eq system-type 'darwin)
       (setq delete-by-moving-to-trash t)
       (setq trash-directory "~/.Trash/")))


;;;; GUI settings

(when (display-graphic-p)
  (menu-bar-mode t)
  (scroll-bar-mode 1)

  ;(setq-default ns-alternate-modifier 'super)


  ; (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'default nil :font "Inconsolata-14")

  (add-to-list 'initial-frame-alist '(left . 1))
  (add-to-list 'initial-frame-alist '(top . 1))
  (add-to-list 'initial-frame-alist
	       (cons 'width
		     (/ (ceiling (* (- (display-pixel-width)
				       (apply '+ (cl-remove-if (lambda (i) (not i))
							       (window-fringes))))
				    1.0))
			(frame-char-width))))
  (add-to-list 'initial-frame-alist (cons 'height (/ (display-pixel-height)
						     (frame-char-height))))
)
(setq-default ansi-color-for-comint-mode t
	      visible-bell t
	      scroll-conservatively 5
	      scroll-margin 5)



;;;; Hippie-expand

(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))



;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;;;; shell mode

(defun my-shell-mode-hook ()
  (setq cursor-type 'box)
  (setq mode-line-format
	(list "%b--" 'default-directory "---%M---%3p---%[(%m: %s)%]%-")))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)


;;;; clojure-mode
(after 'clojure-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode)))

(after 'clojure-mode
  (message "Clojure mode loaded")
  (define-clojure-indent
     (defmulti 'defun)
     (defmethod 'defun)
     (assoc nil)  ; override
     (fact 'defun)
     (facts 'defun)
     (fact-group 'defun)
     (silent-fact 'defun)
     (future-fact 'defun)
     (tabular 'defun)
     (against-background 'defun)
     (error-let 'defun)
     (provided 0)
     )
  (add-hook 'clojure-mode-hook 'hl-sexp-mode)
)



;;;; ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-max-prospects 10)
(setq ido-read-file-name-non-ido nil)
(setq ido-use-filename-at-point nil)
(setq ido-use-virtual-buffers t)


(defun mp-ido-hook ()
  (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map [tab] 'ido-complete))

(add-hook 'ido-setup-hook 'mp-ido-hook)


;;;; ido-ubiquitous
(after 'ido-ubiquitous-autoloads (ido-ubiquitous-mode t))
;(after 'ido-ubiquitous (ido-ubiquitous-disable-in evil-ex))

(setq ido-ubiquitous-command-exceptions '(execute-extended-command))
(setq ido-ubiquitous-function-exceptions '())

;;;; ido-vertical-mode
(after 'ido-vertical-mode-autoloads
  (ido-vertical-mode t))

;;;; smex

(add-hook 'after-init-hook 'smex-initialize)


;;;; Markdown mode
(setq auto-mode-alist
      (cons '("\\.te?xt\\'" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm?d\\'" . markdown-mode) auto-mode-alist))

;;;; Miscellaneous defuns

(defun big ()
  (interactive)
  (text-scale-adjust 1))

(defun line-to-top-of-window ()
  (interactive)
  (recenter 0)
)



;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark blue")))))
