(add-to-list 'load-path "~/.cabal/share")
(add-to-list 'load-path "~/.cabal/bin")

(use-package haskell-mode
  :ensure t
  :init
  (setq haskell-indentation-where-post-offset 2)
  (setq haskell-indentation-where-pre-offset 2)
  )

(use-package intero
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode)
  )

;; (autoload 'hhp-init "hhp" nil t)
;; (autoload 'hhp-debug "hhp" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
;;             (hhp-init)
;;            (intero-targets '("kore:lib" "kore:exe:kore-exec" "kore:exe:kore-format" "kore:exe:kore-parser" "kore:exe:prover" "kore:test:kore-test" "kore:bench:kore-parser-benchmark") t)
            ;; If you have stylish-haskell installed, this will run it with our project's settings whenever you save.
            (setq haskell-stylish-on-save t)
            ))



(provide 'marick-haskell)

