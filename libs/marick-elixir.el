(use-package alchemist
  :ensure t
  :config (save-before '(alchemist-mix-compile
                         alchemist-mix-test
                         alchemist-mix-rerun-last-test
                         alchemist-mix-test-file
                         alchemist-mix-test-this-buffer
                         alchemist-mix-test-at-point
                         alchemist-compile-this-buffer
                         alchemist-compile-file
                         alchemist-compile
                         alchemist-execute-this-buffer
                         alchemist-execute-file
                         alchemist-project-run-tests-for-current-file
                         alchemist-iex-compile-this-buffer
                         alchemist-iex-reload-module))
  )


(defun elixir-visit-source ()
  "If the current line contains text like 'old_procedure_controller_test.exs:57', visit 
that file in the other window and position point on that line."
  (interactive)
  (let* ((start-boundary (save-excursion (beginning-of-line) (point)))
         (regexp (concat "\\([ \t\n\r\"'([<{]\\|^\\)" ; non file chars or
                                                      ; effective
                                                      ; beginning of file  
                         "\\(.+\\.e+xs?\\):\\([0-9]+\\)")) 
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
           (error "No elixir location on line.")))))

(defun my-elixir-mode-hook ()
  (alchemist-mode 1)
  (company-mode 1))
(add-hook 'elixir-mode-hook 'my-elixir-mode-hook)


(provide 'marick-elixir)
;;; marick-elixir.el ends here
