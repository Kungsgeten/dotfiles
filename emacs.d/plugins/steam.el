(require 'url)
(require 'xml)
(require 'cl)

(defvar steam-games nil)
(defvar steam-username nil)

(defun steam-get-xml ()
  (with-current-buffer
      (url-retrieve-synchronously (format "http://steamcommunity.com/id/%s/games?tab=all&xml=1"
                                          steam-username))
    (goto-char url-http-end-of-headers)
    (car (xml-get-children (car (xml-parse-region (point) (point-max)))
                           'games))))

(defun steam-game-attribute (game attribute)
  (caddar (xml-get-children game attribute)))

(defun steam-get-games ()
  (setq steam-games
        (mapcar (lambda (game)
                  (cons (steam-game-attribute game 'name)
                        (steam-game-attribute game 'appID)))
                (xml-get-children (steam-get-xml)
                                  'game))))

(defun steam-launch-id (id)
  (case system-type
    ('windows-nt (shell-command (concat "explorer steam://rungameid/" id)))
    ('gnu/linux (shell-command (concat "steam steam://rungameid/" id)))
    ('darwin (shell-command (concat "open steam://rungameid/" id)))))

(defun steam-insert-org ()
  (interactive)
  (unless steam-games (steam-get-games))
  (mapcar (lambda (game)
            (insert (concat "* [[elisp:(steam-launch-id \""
                            (cdr game) "\")][" (car game) "]]\n")))
          steam-games))

(defun steam-launch ()
  (interactive)
  (unless steam-games (steam-get-games))
  (let ((game (cdr (assoc
                    (ido-completing-read
                     "Game: " steam-games)
                    steam-games))))
    (when game (steam-launch-id game))))

(provide 'steam)
