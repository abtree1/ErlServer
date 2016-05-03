-define(CONFIG_DIR, "config").
-define(CONFIG_INCLUDE_DIR, "include/config.hrl").
-define(CONFIG_INCLUDE_DATA_DIR, "include/config_data.hrl").
-define(CONFIG_DIRTY_WORDS, "config/dirtywords.filter").
-define(CONFIG_INCLUDE_DIRTY_WORDS, "include/dirtywords.hrl").

-define(DATABASE, "erl_server").
-define(DATABASE_U, "root").
-define(DATABASE_P, "root").
-define(DATABASE_H, "127.0.0.1").
-define(DATABASE_O, 3306).
-define(DATABASE_S, 10).
-define(DATABASE_C, utf8).
-define(DATABASE_SQL, "config/db.sql").
-define(DATABASE_HRL, "include/db.hrl").

-define(TCP_LISTEN_PORT, 5555).