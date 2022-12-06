;; My from-scratch emacs config, following along a little bit from the Emacs from Scratch folksm
;; invoke with XDG_CONFIG_HOME=$(pwd) emacs

;; Disable the startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable visible toolbar
(menu-bar-mode -1) ; Disable visible menubar
;; Disable visual tooltips at the cursor, instead displaying them in the 'echo bar' at the bottom.
;; I like it better at the bottom, but perhaps that's just my experience from doom? We can revisit later
(tooltip-mode -1)


;; Set up our font.
;; TODO: Find out what the fuck unit this is. And what does the 'default nil mean?
(set-face-attribute 'default nil :font "JetBrains Mono" :height 180)

;; Set a theme!
;; 'tango-dark is a *symbol*. How is this different from :font earlier?

;; Setu package management
(require 'package) ; This is the built in, meh package manager within emacs
;; TODO: Figure out what this gets stuff from.
(package-initialize) ; Initialize the built in package manager
(unless package-archive-contents
  (package-refresh-contents)) ; Equivalent of apt-get update
;; Add MELPA to list of package sources. I *think* this is the one where you don't have
;; to sign your soul away to RMS to publish in.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install use-package, which is the package manager we actually wanna use
;; But only if it isn't already installed!
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

;; Setup use-package
(require 'use-package)
;; This makes :ensure be t by default for all use-package macro calls, so if the package
;; is not currently in the system it *will* be installed. This is the behavior I want, as
;; otherwise I've to have a separate setup for installation and configuration.
;; https://github.com/jwiegley/use-package#package-installation
;; Apparently this does *not* keep it up to date or check versions. Which seems fair enough,
;; as otherwise you'd need to do a remote call every startup!
(setq use-package-always-ensure t)

;; Setup completion package
;; This by default seems to enable completion in find-file, M-x, etc.
;; FIXME: Put some keybindings here? But I don't want emacs keybindings
(use-package ivy
  :config
  (ivy-mode 1)
  )

;; Use same modeline that doom uses! But simpler to start with, for some reason.
;; We can customize this later
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  )

;; Use doom-themes probably - apparently they are also integrated into other themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-henna t)  ; The second param being t marks it as safe
  )

;; Provide icon packs for doom-themes and other packages
;; FIXME: Find out how to automatically install these if needed?
(use-package all-the-icons)

;; Auto-save
;; FIXME: What does *this* actually mean?
(setq auto-save-default t)
; Auto-save *everything* whenever we lose focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; Enable line numbers!
(global-display-line-numbers-mode t)
;; Display colum numbers too
(column-number-mode)
;; Disable line numbers in some modes
;; dolist is a bit like for-each, and this lets us turn it off for many modes
(dolist (mode '(term-mode-hook vterm-mode-hook)) (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Run rainbow-delimiters-mode for all *programming* modes
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key is what gives the super helpful pane at the bottom from doom-emacs / spacemacs!!!!!!
;; My god, it is amazing!
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  )

;; Provide helpful info next to ivy commands maybe?
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Use counsel for everything by default, so we get the rich help text from ivy-rich
;; FIXME: Should set more here once we get into evil mode, as that's the way I wann use emacs
(use-package counsel
  :bind (("M-x" . counsel-M-x))
  :config
  (setq ivy-initial-inputs-alist nil) ; Do fuzzy searches in all lists by default, not exact ones.
                                        ; Without this, it all starts with ^, so whatever you type becomes a prefix search
  ;; FIXME: Should this be here, or in use-package for helpful? It didn't work when it was there, so not sure.
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  )

;; Provide better formated *elisp* help
(use-package helpful
  :bind ; Using [remap] will just respect whatever describe-function is set to elsewhere.
  ;; We're using the counsel versions here, as that gives us autocomplete for list of functions or variables
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  )


;; Start every frame maximized
;; From https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; EVIL MODE HERE WE FUCKING COME BITCHES!

(use-package evil
  :init
  (setq evil-want-integration t) ; Integrate with something? FIXME: Find out what we are integrating with?
  (setq evil-want-keybinding nil) ; Don't add some random keybindings? idk what that is either.
  :config
  (evil-mode 1)

  ;; Make j and k work on what looks like lines on screen, not what looks like lines on text
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  )
;;
;; Setup general for keymaps
(use-package general
  :config
  ;; Tell it we wanna use it with evil
  (general-evil-setup t)

  ;; Set <escape> to mean the same as C-g in *most* places
  ;; This doesn't account for magit though, that is handled in the magit block
  (general-define-key
   "<escape>" 'keyboard-escape-quit
   )

  ;; Define a custom function that can be used to define our custom keymaps
  (general-create-definer yuvi/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    )

  ;; THIS IS WHERE OUR KEYBINDINGS ARE OMG OMG OMG
  ;; Let's keep this as minimal as possible, and rely on M-x for things we won't do often (like switching themes)
  (yuvi/leader-keys
   ;; TOP LEVEL KEYBINDINGS FUCK YES!!!!!! OMG
   ";" '(counsel-M-x :which-key "M-x")
   "SPC" '(counsel-switch-buffer :which-key "Switch buffers")

   ;; Window specific keybindings. In the non-emacs world, i'd call these 'frames' lol
   "w" '(:ignore t :which-key "window")
   "wv" '(split-window-vertically :which-key "vertical-split")
   "wh" '(split-window-horizontally :which-key "horizontal-split")
   "wd" '(delete-window :which-key "close")
   "wn" '(evil-window-next :which-key "next")
   "wp" '(evil-window-prev :which-key "previous")
   ;; TODO: Maximize current window
   )
  )

;; Sets up evil keybindings for a lot of modes, like helpful
(use-package evil-collection
  :after evil
  :init
  (evil-collection-init)
  )


;; Project support
(use-package projectile
  :init
  (projectile-mode 1)
  :config
  ;; SET PATH WHERE PROJECTS ARE! 
  (setq projectile-project-search-path '("~/code/"))
  ;; Enable caching to make things not super slow
  (setq projectile-enable-caching t)
  (yuvi/leader-keys
   ;; TOP LEVEL KEYBINDINGS FUCK YES!!!!!! OMG
    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pf" '(projectile-find-file-in-known-projects :which-key "find file in all projects")
    "pi" '(projectile-invalidate-cache :which-key "invalidate cache")
    "pd" '(projectile-discover-projects-in-search-path :which-key "discover projects")
    "ps" '(counsel-projectile-rg :which-key "search")

    ;; This is so frequently used, it is top level
    "," '(projectile-find-file :which-key "find file in project")
    )
  )


;; MAGIT!
(use-package magit
  :config
  (yuvi/leader-keys
    ;; Unlike doomm, let's just stick to one g here
    ;; I don't think I've ever literally used any of the other commands there?
    "g" '(magit-status :which-key "git status")
    )
  ;; Make sure that <ESC> quits magit transient things (like ?)
  ;; From https://github.com/emacs-evil/evil-magit/issues/14#issuecomment-626583736
  (general-define-key
   :keymaps 'transient-base-map
   "<escape>" 'transient-quit-one)
  )

;; Setup forge to work with GitHub
(use-package forge
  :after magit)


;; Setup our terminal things
(use-package vterm
  :config
  (yuvi/leader-keys
    ;; I don't think I use other aspects of 't' in doom so much
    "t" '(projectile-run-vterm :which-key "project terminal")
    )
)

;; Setup search
(use-package counsel-projectile
  :after projectile
  )

;; Setup language modes as we come across their needs
(use-package yaml-mode)
(use-package terraform-mode)


;; Automatic environment activation
;;
;; Our venvs are stored under ~/.virtualenvs, with the name of the project used for
;; the name of the venv. We figure out the correct one and activate it each time we
;; switch into a python mode
;; Stolen from https://ddavis.io/posts/emacs-python-lsp/
(defun py-workon-project-venv ()
"Call pyenv-workon with the current projectile project name.
This will return the full path of the associated virtual
environment found in $WORKON_HOME, or nil if the environment does
not exist."
(let ((pname (projectile-project-name)))
    (pyvenv-workon pname)
    (if (file-directory-p pyvenv-virtual-env)
	pyvenv-virtual-env
    (pyvenv-deactivate))))


;; Python setups
(use-package pyvenv
  :config
  ;; Set appropriate python virtual env each time we do any window changes
  (add-hook 'window-configuration-change-hook 'py-workon-project-venv)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(package-selected-packages
   '(counsel-projectile rg ripgrep terraform-mode pyvenv vterm yaml-mode forge magit projectile evil-collection counsel which-key use-package rainbow-delimiters ivy-rich doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
