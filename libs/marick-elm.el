(use-package f :ensure)
(use-package let-alist :ensure)
(use-package s :ensure)

(use-package elm-mode
  :ensure t
  :init
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)
  ; (elm-indent-mode false)
  (setq elm-indent-offset 2))
             

(use-package flycheck
  :init (global-flycheck-mode)
  :ensure t)

;; (use-package flycheck-elm
;;   :ensure t
;;   :init (add-hook 'flycheck-mode-hook #'flycheck-elm-setup)
;;         (add-hook 'after-init-hook #'global-flycheck-mode))


(provide 'marick-elm)
;;; marick-elm.el ends here
