-define(DB_TABLE_NAMES, [user,item,alliance]).
-record(user, {uuid,name,level,account,passwd,alliance_id}).
-record(item, {uuid,name,amount,type}).
-record(alliance, {uuid,name,level,coins,president,officers,elites,members}).
