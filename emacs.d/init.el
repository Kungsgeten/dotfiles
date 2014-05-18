(package-initialize)
(setq package-enable-at-startup nil)
(require 'org-install)
(require 'ob-tangle)

(org-babel-load-file "~/.emacs.d/evilinit.org")
