;;; init.el --- Derived from Milkmacs configuration file
;;; http://milkbox.net/note/single-file-master-emacs-configuration/


;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; (setq inhibit-startup-screen t)
; (setq inhibit-startup-message t)

(setenv "EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
(setenv "PAGER" "cat")

;; Commands that trip me up - or not 

(put 'downcase-region 'disabled t)
(put 'upcase-region 'disabled t)
(put 'narrow-to-region 'disabled nil)

(put 'eval-expression 'disabled nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(put 'set-goal-column 'disabled nil)

(push "~/.emacs.d/libs" load-path)

;;;; Miscellaneous defuns

(defun big ()
  (interactive)
  (text-scale-adjust 1))

(defun line-to-top-of-window ()
  (interactive)
  (recenter 0))

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(defun save-before (commands)
  (dolist (c commands) 
    (advice-add c :before (lambda (&rest ignored) (save-some-buffers t)))))

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)



(package-initialize)


(defvar bem-packages 
  '(exec-path-from-shell
    browse-kill-ring
    clojure-mode
    ace-jump-mode
    browse-kill-ring
    expand-region
    gist
    iy-go-to-char
    hl-sexp
    markdown-mode+
    rainbow-delimiters
    company
    clj-refactor
    reason-mode
;    midje-mode
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
(require 'uniquify) 
(setq uniquify-buffer-name-style 'forward
      uniquify-ask-about-buffer-names-p t
      uniquify-ignore-buffers-re "^\\*")
(require 'checkdoc)
(require 'clojure-jump-to-file)
(require 'dedicate-windows-manually)


(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;;;; Paths

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
(setq-default backup-directory-alist '(("." . "~/.emacs-backups")))
(setq ag-reuse-buffers 't)

;;;; GUI settings

(when (display-graphic-p)
  (menu-bar-mode t)
  (scroll-bar-mode 1)

  (setq-default ns-alternate-modifier 'super) 
  (setq-default ns-command-modifier 'hyper) 

  ; (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'default nil :font "Menlo-14")

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
              scroll-conservatively 5
              scroll-margin 5)



;;;; shell commands

(defun do-command-from-here (shell-buffer cmd)
  (interactive)
  (save-all-buffers)
  (switch-to-buffer-other-window shell-buffer)
  (goto-char (point-max))
  (insert cmd)
  (comint-send-input))

(defun shell-command-again ()
  (interactive)
  (do-command-from-here  "*shell*" "!!"))

(defun ruby-shell-command-again ()
  (interactive)
  (do-command-from-here  "*shell*" "!ruby"))

(defun rake-shell-command-again ()
  (interactive)
  (do-command-from-here  "*shell*" "!rake"))

(defun open-shell-command-again ()
  (interactive)
  (do-command-from-here  "*shell*" "!open"))

(defun lein-shell-command-again ()
  (interactive)
  (do-command-from-here "*clojure*" "!lein"))



;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;;;; midje mode

;; (push "~/src/midje/midje-mode" load-path)
;; (require 'midje-mode)
;; (require 'clojure-jump-to-file)

;; (require 'clj-refactor)
;; (defun my-clojure-mode-hook ()
;;   (midje-mode 1)
;;   (clj-refactor-mode 1)
;;   (yas-minor-mode 1) ; for adding require/use/import
;;   (cljr-add-keybindings-with-prefix "C-c C-m")
;;   (company-mode-on)
;;   )

;; (add-hook 'clojure-mode-hook 'my-clojure-mode-hook)

;;;; shell mode

(defun my-shell-mode-hook ()
  (setq cursor-type 'box)
  (setq mode-line-format
	(list "%b--" 'default-directory "---%M---%3p---%[(%m: %s)%]%-")))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)


;;;; pony

(require 'ponylang-mode)
(add-hook
  'ponylang-mode-hook
  (lambda ()
    (set-variable 'indent-tabs-mode nil)
    (set-variable 'tab-width 2)))



;;;; clojure-mode
(after 'clojure-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode)))

(defun clojure-generic-expression ()
  (setq imenu-prev-index-position-function nil)
  (setq imenu-generic-expression '((nil "^\(facts? \\(.*\\)" 1))))

(defun clojure-autotest ()
  (interactive)
  (save-all-buffers)
  (when (string= "*" (substring (buffer-name (window-buffer (k-window))) 0 1))
    (save-window-excursion
      (switch-to-k-window)
      (end-of-buffer))))

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
     (when-maybe 'defun)
     (when-some 'defun)
     (provided 0)
     )
  (add-hook 'clojure-mode-hook 'hl-sexp-mode)
  (add-hook 'clojure-mode-hook 'clojure-generic-expression)
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
             (goto-line (string-to-number line))))
          (t
           (error "No ruby location on line.")))))

(add-hook 'ruby-mode-hook
          '(lambda ()
;             (hl-line-mode)
             (company-mode-on)))

;;;; Markdown mode
(setq auto-mode-alist
      (cons '("\\.te?xt\\'" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm?d\\'" . markdown-mode) auto-mode-alist))





(defun h-window () (window-at 1 1))
(defvar h-buffer nil)

(defun j-window () (window-at 150 1))
(defvar j-buffer nil)

(defun k-window () (window-at 1 25))
(defvar k-buffer nil)

(defun l-window () (window-at 150 25))
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

(global-set-key (kbd "H-s-h") 'find-file-in-h-window) 
(global-set-key (kbd "H-s-j") 'find-file-in-j-window)
(global-set-key (kbd "H-s-k") 'find-file-in-k-window)
(global-set-key (kbd "H-s-l") 'find-file-in-l-window)

(global-set-key (kbd "H-s-g") 'four-windows)
(global-set-key (kbd "H-s-s") 'clojure-autotest)




(global-set-key (kbd "C-h l") 'ace-jump-line-mode)
(global-set-key (kbd "C-h C-d") 'ace-jump-word-mode)

(global-set-key (kbd "H-w") 'er/expand-region)

(global-set-key (kbd "H-c") 'ns-copy-including-secondary)
(global-set-key (kbd "H-x") 'kill-region)
(global-set-key (kbd "H-v") 'yank)
(global-set-key (kbd "H-z") 'undo)
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "C-x s") 'save-all-buffers)
(global-set-key (kbd "H-a") 'mark-whole-buffer)


(global-set-key (kbd "C-c C-f") 'iy-go-to-char)
(global-set-key (kbd "C-c C-b") 'iy-go-to-char-backward)


(global-set-key "\e!" 'line-to-top-of-window)
(global-set-key (kbd "C-x C-l") 'goto-line) 
 
(global-set-key [f4] 'shell-command-again)
(global-set-key [f5] 'ruby-shell-command-again)
(global-set-key [f6] 'rake-shell-command-again)
(global-set-key [f7] 'open-shell-command-again)
(global-set-key (kbd "H-m") 'lein-shell-command-again)

(global-set-key "\^h\^h" 'ruby-visit-source)    
(global-set-key "\^he" 'elixir-visit-source)    


;;; Healthfinch
(push "~/lib/emacs/emacs-common-denominator" load-path)
(require 'healthfinch-init)
(remove-hook 'after-init-hook 'global-company-mode)

;;; Me
; (require 'marick-elm)
; (require 'marick-purescript)
; (require 'marick-reason)
; (require 'marick-haskell)
(require 'marick-elixir)

(ignore-errors
  (server-start))
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(purescript-mode-hook
   (quote
    (capitalized-words-mode turn-on-purescript-indentation))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-sexp-face ((t (:background "gray14"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark magenta"))) t)
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark red"))) t)
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark blue"))) t))
