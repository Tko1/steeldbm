;;dbm - database manager, until a better name has been found
(defun dbm/new-database (user pass db)
  `((:username . ,user) (:password . ,pass) (:database . ,db)))
(defvar test-database (dbm/new-database "root" "password" "obsidian-chat"))
(assert (equal (dbm/new-database "root" "password" "obsidian-chat")
               '((:username . "root")
                 (:password . "password")
                 (:database . "obsidian-chat"))))
(assert (equal test-database
               (dbm/new-database "root" "password" "obsidian-chat")))
(defvar +database+
  (dbm/new-database "root" "password" "obsidian-chat"))
