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
(defun dbm/db-exec (db command)
  (shell-command-to-string
   (concat
    "mysql "
    "-u" (alist-get :username db) " "
    "-p" (alist-get :password db) " "
    (alist-get :database db) " "
    "-e"
    "'" command "'")))
(assert (s-match "user_id" (dbm/db-exec test-database "show columns from messages")))
(assert (s-match "channel_id" (dbm/db-exec test-database "show columns from messages")))
(assert (not (s-match "ass_id" (dbm/db-exec test-database "show columns from messages"))))

(defun dbm/raw-show-db-columns (db table)
  (dbm/db-exec db (concat "show columns from " table)))
(assert (s-match "user_id" (dbm/raw-show-db-columns test-database "messages")))
(assert (s-match "channel_id" (dbm/raw-show-db-columns test-database "messages")))
(assert (not (s-match "ass_id" (dbm/raw-show-db-columns test-database "messages"))))

(defun dbm/get-db-columns (db table)
  (->>
   (split-string (dbm/raw-show-db-columns db table) "\n")
   rest
   rest
   (mapcar (lambda (x) (-> x split-string car)))
   (-drop-last 1)))
(assert (equal '("id" "user_id" "channel_id" "message" "created_at") (dbm/get-db-columns test-database "messages")))
