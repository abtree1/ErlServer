-define(CONFIG_DIR, "config").
-define(CONFIG_INCLUDE_DIR, "include/auto/config.hrl").
-define(CONFIG_INCLUDE_DATA_DIR, "include/auto/config_data.hrl").
-define(CONFIG_DIRTY_WORDS, "config/dirtywords.filter").
-define(CONFIG_INCLUDE_DIRTY_WORDS, "include/auto/dirtywords.hrl").

-define(DATABASE, "erl_server").
-define(DATABASE_U, "root").
-define(DATABASE_P, "root").
-define(DATABASE_H, "127.0.0.1").
-define(DATABASE_O, 3306).
-define(DATABASE_S, 10).
-define(DATABASE_C, utf8).
-define(DATABASE_SQL, "config/db.sql").
-define(DATABASE_HRL, "include/auto/db.hrl").

-define(STATE_MODEL_CREATE, 1).
-define(STATE_MODEL_LOAD, 2).
-define(STATE_MODEL_DELETE, 3).

-define(TCP_LISTEN_PORT, 5555).

-define(PROTOCOL_UTIL_FILE, "config/protocol.pro").
-define(PROTOCOL_CONTROLLER_FILE, "config/pcontrollers.pro").
-define(PROTOCOL_ENUM, "include/auto/proto_enum.hrl").
-define(PROTOCOL_RULES, "include/auto/proto_rules.hrl").
-define(PROTOCOL_CONTROLLER, "include/auto/proto_controller.hrl").

-define(MAP_INCLUDE_DATA, "include/auto/map_data.hrl").
% 六边形坐标取双值，故坐标比四边形大一倍
-define(MAP_TYPE, hexagon).
-define(HEX_MAX_SIZE, 2000). 
-define(HEX_GROUP_SIZE, 20).
-define(RECT_MAX_SIZE, 1000).
-define(RECT_GROUP_SIZE, 10).