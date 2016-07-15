(use-package f :ensure)
(use-package let-alist :ensure)
(use-package s :ensure)

(use-package elm-mode
  :ensure
  :init (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(provide 'marick-elm)
