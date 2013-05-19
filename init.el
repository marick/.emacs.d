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
(setq-default indent-tabs-mode nil)
(put 'set-goal-column 'disabled nil)


(push "~/lib/emacs" load-path)
(push "~/src/midje-mode" load-path)


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
    midje-mode
    nrepl
    ))


(defun bem-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
	bem-packages))

(require 'cl)
(require 'misc)
(require 'midnight)
(require 'saveplace) (setq-default save-place t)
(require 'uniquify) (progn
		      uniquify-buffer-name-style 'forward
		      uniquify-ask-about-buffer-names-p t
		      uniquify-ignore-buffers-re "^\\*")
(require 'checkdoc)
(require 'ido)
(require 'clojure-jump-to-file)



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

  (setq-default ns-alternate-modifier 'super) 
  (setq-default ns-command-modifier 'hyper) 

  ; (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'default nil :font "Inconsolata-14")

  (add-to-list 'initial-frame-alist '(left . 1))
  (add-to-list 'initial-frame-alist '(top . 1))
  (add-to-list 'initial-frame-alist
	       (cons 'width
		     (/ (ceiling (* (- (display-pixel-width)
				       (apply '+ (cl-remove-if (lambda (i) (not i))
							       (window-fringes))))
				    0.99))
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

;;;; shell commands

(defun do-command-from-here (cmd)
  (interactive)
  (save-some-buffers t)
  (switch-to-buffer-other-window "*shell*")
  (goto-char (point-max))
  (insert cmd)
  (comint-send-input))

(defun shell-command-again ()
  (interactive)
  (do-command-from-here "!!"))

(defun ruby-shell-command-again ()
  (interactive)
  (do-command-from-here "!ruby"))

(defun rake-shell-command-again ()
  (interactive)
  (do-command-from-here "!rake"))

(defun open-shell-command-again ()
  (interactive)
  (do-command-from-here "!open"))



;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;;;; midje mode
(add-hook 'clojure-mode-hook 'midje-mode)

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

;;;; Ruby mode 

(defun ruby-visit-source ()
  "If the current line contains text like '../src/program.rb:34', visit 
that file in the other window and position point on that line."
  (interactive)
  (let* ((start-boundary (save-excursion (beginning-of-line) (point)))
         (regexp (concat "\\([ \t\n\r\"'([<{]\\|^\\)" ; non file chars or
                                                      ; effective
                                                      ; beginning of file  
                         "\\(.+\\.rb\\):\\([0-9]+\\)")) ; file.rb:NNN
         (matchp (save-excursion
                  (end-of-line)
                  ;; if two matches on line, the second is most likely
                  ;; to be useful, so search backward.
                  (re-search-backward regexp start-boundary t))))
    (cond (matchp
           (let ((file (buffer-substring (match-beginning 2)
                                         (match-end 2))) 
                 (line (buffer-substring (match-beginning 3)
                                         (match-end 3))))
             ; Windows: Find-file doesn't seem to work with Cygwin
             ; //<drive>/ format or the odd /cygdrive/<drive>/ format 
             (if (or (string-match "//\\(.\\)\\(.*\\)" file)
                     (string-match "/cygdrive/\\(.\\)\\(.*\\)" file))
                 (setq file
                       (concat (substring file
                                          (match-beginning 1)
                                          (match-end 1))
                               ":"
                               (substring file
                                          (match-beginning 2)
                                          (match-end 2)))))
                             
             (find-file-other-window file)
             (goto-line (string-to-int line))))
          (t
           (error "No ruby location on line.")))))

(add-hook 'ruby-mode-hook '(lambda () (hl-line-mode)))


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
  (recenter 0))

(defun h-window () (window-at 1 1))
(defvar h-buffer nil)

(defun j-window () (window-at 150 1))
(defvar j-buffer nil)

(defun k-window () (window-at 1 50))
(defvar k-buffer nil)

(defun l-window () (window-at 150 50))
(defvar l-buffer nil)

(defun switch-to-foo-window (window)
  (select-window (funcall window)))
(defun switch-to-h-window () (interactive) (switch-to-foo-window 'h-window))
(defun switch-to-j-window () (interactive) (switch-to-foo-window 'j-window))
(defun switch-to-k-window () (interactive) (switch-to-foo-window 'k-window))
(defun switch-to-l-window () (interactive) (switch-to-foo-window 'l-window))

(defun put-buffer-in-foo-window (window memory)
  (switch-to-foo-window window)
  (ido-switch-buffer)
  (remember-buffer memory))
(defun put-buffer-in-h-window () (interactive) (put-buffer-in-foo-window 'h-window 'h-buffer))
(defun put-buffer-in-j-window () (interactive) (put-buffer-in-foo-window 'j-window 'j-buffer))
(defun put-buffer-in-k-window () (interactive) (put-buffer-in-foo-window 'k-window 'k-buffer))
(defun put-buffer-in-l-window () (interactive) (put-buffer-in-foo-window 'l-window 'l-buffer))

(defun find-file-in-foo-window (window memory)
  (switch-to-foo-window window)
  (ido-find-file)
  (remember-buffer memory))
(defun find-file-in-h-window () (interactive) (find-file-in-foo-window 'h-window 'h-buffer))
(defun find-file-in-j-window () (interactive) (find-file-in-foo-window 'j-window 'j-buffer))
(defun find-file-in-k-window () (interactive) (find-file-in-foo-window 'k-window 'k-buffer))
(defun find-file-in-l-window () (interactive) (find-file-in-foo-window 'l-window 'l-buffer))

(defun put-recent-file-in-foo-window (window memory)
  (switch-to-foo-window window)
  (recentf-open-files)
  (remember-buffer))
(defun put-recent-file-in-h-window () (interactive) (put-recent-file-in-foo-window 'h-window 'h-buffer))
(defun put-recent-file-in-j-window () (interactive) (put-recent-file-in-foo-window 'j-window 'j-buffer))
(defun put-recent-file-in-k-window () (interactive) (put-recent-file-in-foo-window 'k-window 'k-buffer))
(defun put-recent-file-in-l-window () (interactive) (put-recent-file-in-foo-window 'l-window 'l-buffer))


(defun remember-buffer (memory)
  (set memory (buffer-name (window-buffer))))
  
(defun reopen-buffer (buffer window)
  (cond ((eq buffer nil))
        ((get-buffer buffer)
         (switch-to-foo-window window)
         (set-window-buffer nil (get-buffer buffer)))
        ((assoc buffer ido-virtual-buffers)
         (switch-to-foo-window window)
         (find-file (cdr (assoc buffer ido-virtual-buffers))))))
  
(defun four-windows ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (split-window-horizontally)
  (switch-to-l-window)
  (split-window-horizontally)
  (map 'list #'reopen-buffer
       (list h-buffer j-buffer k-buffer l-buffer)
       (list 'h-window 'j-window 'k-window 'l-window)))

  
;;;; global key bindings

(windmove-default-keybindings 'control)

(global-set-key "\e%" 'query-replace-regexp)
(global-set-key (kbd "C-c b") 'ido-switch-buffer-other-window)


(global-set-key (kbd "s-h") 'switch-to-h-window) 
(global-set-key (kbd "s-j") 'switch-to-j-window)
(global-set-key (kbd "s-k") 'switch-to-k-window)
(global-set-key (kbd "s-l") 'switch-to-l-window)

(global-set-key (kbd "H-h") 'put-buffer-in-h-window) 
(global-set-key (kbd "H-j") 'put-buffer-in-j-window)
(global-set-key (kbd "H-k") 'put-buffer-in-k-window)
(global-set-key (kbd "H-l") 'put-buffer-in-l-window)

(global-set-key (kbd "C-x H-l") 'find-file-in-h-window) 
(global-set-key (kbd "C-x H-j") 'find-file-in-j-window)
(global-set-key (kbd "C-x H-k") 'find-file-in-k-window)
(global-set-key (kbd "C-x H-l") 'find-file-in-l-window)


(global-set-key (kbd "ESC <f1>") 'put-recent-file-in-h-window) 
(global-set-key (kbd "ESC <f2>") 'put-recent-file-in-j-window)
(global-set-key (kbd "ESC <f3>") 'put-recent-file-in-k-window)
(global-set-key (kbd "ESC <f4>") 'put-recent-file-in-l-window)



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

(global-set-key (kbd "H-w") 'er/expand-region)

(global-set-key (kbd "H-c") 'ns-copy-including-secondary)
(global-set-key (kbd "H-x") 'kill-region)
(global-set-key (kbd "H-v") 'yank)
(global-set-key (kbd "H-z") 'undo)
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-a") 'mark-whole-buffer)


(global-set-key (kbd "C-c C-f") 'iy-go-to-char)
(global-set-key (kbd "C-c C-b") 'iy-go-to-char-backward)

(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key [f8] 'hippie-expand)

(global-set-key "\e!" 'line-to-top-of-window)
(global-set-key (kbd "C-x C-l") 'goto-line) 
 
(global-set-key [f4] 'shell-command-again)
(global-set-key [f5] 'ruby-shell-command-again)
(global-set-key [f6] 'rake-shell-command-again)
(global-set-key [f7] 'open-shell-command-again)

(global-set-key "\^h\^h" 'ruby-visit-source)    





;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (wheatgrass))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-sexp-face ((t (:background "gray24"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark magenta"))) t)
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark red"))) t)
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark blue"))) t))
