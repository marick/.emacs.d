(use-package f)
(use-package let-alist)
(use-package s)

(use-package elm-mode
  :init (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(provide 'marick-elm)
